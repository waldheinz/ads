
{-# LANGUAGE OverloadedStrings #-}

module Node (
  -- * our node
  Node, mkNode,
  requestNodeData, nodeArchives,
  
  -- * peers
  ConnectFunction, initPeers,
  
  -- * other nodes we're connected to
  PeerNode, runPeerNode  
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent ( forkIO, killThread )
import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBMQueue as STM
import Control.Monad ( forever, unless, void, when )
import Control.Monad.IO.Class ( liftIO )

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.TQueue as C
import Data.List ( (\\) )
import qualified Data.Map.Strict as Map
import Data.Maybe ( isJust )
import qualified Data.Text as T
import System.FilePath ( (</>) )
import System.Log.Logger

import qualified Freenet as FN
import qualified Freenet.Archive as FN
import qualified Freenet.Chk as FN
import qualified Freenet.Ssk as FN
import qualified Freenet.Types as FN
import qualified Freenet.URI as FN
import Message as MSG
import qualified NextBestOnce as NBO
import Types

logI :: String -> IO ()
logI m = infoM "node" m

logW :: String -> IO ()
logW m = warningM "node" m

-------------------------------------------------------------------------
-- Node
-------------------------------------------------------------------------

data Node a = Node
            { nodePeers    :: Peers a
            , nodeIdentity :: Peer a
            , nodeMidGen   :: MessageIdGen
            , nodeActMsgs  :: TVar (Map.Map MessageId (PeerNode a))          -- ^ messages we're currently routing
            , nodeNbo      :: NBO.Node NodeId (RoutedMessage a) (PeerNode a) -- ^ our NBO identity for routing
            , nodeFreenet  :: FN.Freenet a                                   -- ^ our freenet compatibility layer
            , nodeIncoming :: TChan (PeerNode a, Message a)                  -- ^ stream of incoming messages
            , nodeArchives :: FN.ArchiveCache
            }

mkNode :: (Show a) => Peer a -> FN.Freenet a -> IO (Node a)
mkNode self fn = do
  peers <- atomically $ mkPeers
  midgen <- mkMessageIdGen
  msgMap <- newTVarIO Map.empty
  incoming <- newBroadcastTChanIO
  ac <- FN.mkArchiveCache 10
  
  let
    nbo = NBO.Node
        { NBO.location          = nodeId $ peerNodeInfo self
        , NBO.neighbours        = readTVar $ peersConnected peers
        , NBO.neighbourLocation = \np -> nodeId $ peerNodeInfo $ nodePeer np
        , NBO.popPred           = \msg -> messagePopPred node (rmId msg)
        , NBO.addPred           = \msg n -> modifyTVar' msgMap $ Map.insert (rmId msg) n
        , NBO.routingInfo       = rmInfo
        , NBO.updateRoutingInfo = \rm ri -> rm { rmInfo = ri }
        }

    node = Node peers self midgen msgMap nbo fn incoming ac
    
  writeToStores node
  handleFreenetRequests node
  return node

waitResponse :: Node a -> MessageId -> IO (TMVar (Maybe (MessagePayload a)))
waitResponse node mid = do
  timeout <- registerDelay (30 * 1000 * 1000)
  bucket  <- newEmptyTMVarIO
  chan    <- atomically $ dupTChan (nodeIncoming node)

  let
    doWait = orElse
      (readTChan chan   >>= checkResponse)
      (readTVar timeout >>= \to -> if to then putTMVar bucket Nothing else retry)
    
    checkResponse msg = case msg of
      (_, (Response mid' rp)) -> if mid == mid' then (putTMVar bucket $ Just rp) else doWait
      _                       -> doWait

  -- wait for data or timeout
  void $ forkIO $ atomically $ doWait
  return bucket

-- |
-- Turn a Freenet @Key@ into a @NodeId@ by repacking the 32 bytes.
keyToTarget :: FN.Key -> NodeId
keyToTarget key = mkNodeId' $ FN.unKey key

-- |
-- Handle a data request at node level. This means we first ask our Freenet layer
-- if it has the data in it's stores, and if this fails we route a message to our
-- peer nodes.
requestChk :: (Show a) => Node a -> FN.ChkRequest -> IO (Either T.Text FN.ChkBlock)
requestChk n req = do
  local <- FN.getChk (nodeFreenet n) req
  case local of
    Right blk -> return $ Right blk
    Left _    -> do
--      logI $ "could not fetch data locally " ++ show e
      msg <- mkRoutedMessage n (keyToTarget $ FN.dataRequestLocation req) (FreenetChkRequest req)
      bucket <- waitResponse n $ rmId msg
      sendRoutedMessage n msg Nothing
      result <- atomically $ takeTMVar bucket
      case result of
        Nothing   -> return $ Left "timeout waiting for response"
        Just resp -> case resp of
          (FreenetChkBlock blk) -> return $ Right blk
          x                     -> return $ Left $ "expected CHK block, but got: " `T.append` (T.pack $ show x)

requestSsk :: (Show a) => Node a -> FN.SskRequest -> IO (Either T.Text FN.SskBlock)
requestSsk n req = do
  local <- FN.getSsk (nodeFreenet n) req
  case local of
    Right blk -> return $ Right blk
    Left _    -> do
--      logI $ "could not fetch data locally " ++ show e
      msg <- mkRoutedMessage n (keyToTarget $ FN.dataRequestLocation req) (FreenetSskRequest req)
      bucket <- waitResponse n $ rmId msg
      sendRoutedMessage n msg Nothing
      result <- atomically $ takeTMVar bucket
      case result of
        Nothing   -> return $ Left "timeout waiting for response"
        Just resp -> case resp of
          (FreenetSskBlock blk) -> return $ Right blk
          x                     -> return $ Left $ "expected SSK block, but got: " `T.append` (T.pack $ show x)

mkRoutedMessage :: Node a -> NodeId -> MessagePayload a -> IO (RoutedMessage a)
mkRoutedMessage node target msg = atomically $ do
  mid <- nextMessageId $ nodeMidGen node
  return $ RoutedMessage msg mid $ NBO.mkRoutingInfo target

sendRoutedMessage :: Show a => Node a -> RoutedMessage a -> Maybe (PeerNode a) -> IO ()
sendRoutedMessage node msg prev = do
  mlog <- atomically $ do
    result <- NBO.route (nodeNbo node) prev msg
    case result of
      NBO.Forward dest msg'  -> enqMessage dest (Routed msg') >> return Nothing
--      NBO.Backtrack des msg'
      NBO.Fail               -> return $ Just $ logI $ "message failed fatally: " ++ show msg
      
  case mlog of
    Nothing -> return ()
    Just l  -> l

messagePopPred :: Node a -> MessageId -> STM (Maybe (PeerNode a))
messagePopPred node mid = do
  (tgt, m') <- Map.updateLookupWithKey (\_ _ -> Nothing) mid <$> readTVar (nodeActMsgs node)
  writeTVar (nodeActMsgs node) m'
  return tgt
  
forwardResponse :: Node a -> MessageId -> MessagePayload a -> IO ()
forwardResponse node mid msg = do
  mtgt <- atomically $ messagePopPred node mid
  
  case mtgt of
    Nothing -> logW $ "could not send response, message id unknown: " ++ show mid
    Just pn -> atomically $ enqMessage pn $ Response mid msg

handleFreenetRequests :: Show a => Node a -> IO ()
handleFreenetRequests node = do
  chan <- atomically $ dupTChan $ nodeIncoming node

  let
    fn = nodeFreenet node
  
  void $ forkIO $ forever $ do
    (pn, msg) <- atomically $ readTChan chan

    case msg of
      Routed rm@(RoutedMessage (FreenetChkRequest req) mid _) -> do
        local <- FN.getChk fn req
        case local of
          Left _    -> sendRoutedMessage node rm (Just pn) -- pass on
          Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetChkBlock blk

      Routed rm@(RoutedMessage (FreenetSskRequest req) mid _) -> do
        local <- FN.getSsk fn req
        case local of
          Left _    -> sendRoutedMessage node rm (Just pn) -- pass on
          Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetSskBlock blk

      Response mid msg' -> forwardResponse node mid msg'

      _ -> return ()
      
-- |
-- Installs a incoming message handler which will offer all data blocks
-- coming along this node to the Freenet subsystem, which may then put
-- them in it's store(s) when it sees fit.
writeToStores :: Show a => Node a -> IO ()
writeToStores node = do
  chan <- atomically $ dupTChan $ nodeIncoming node

  void $ forkIO $ forever $ do
    (_, msg) <- atomically $ readTChan chan

    case msg of
      Response _ (FreenetChkBlock blk) -> FN.offerChk (nodeFreenet node) blk
      Response _ (FreenetSskBlock blk) -> FN.offerSsk (nodeFreenet node) blk
      _                                -> return ()

type ConnectFunction a = Peer a -> ((Either String (MessageIO a)) -> IO ()) -> IO ()

----------------------------------------------------------------
-- the peers list
----------------------------------------------------------------

data Peers a = Peers
               { peersConnected     :: TVar [PeerNode a] -- ^ peers we're currently connected to
               , peersConnecting    :: TVar [Peer a]     -- ^ peers we're currently trying to connect to
               , peersKnown         :: TVar [Peer a]     -- ^ all peers we know about, includes above
               }

mkPeers :: STM (Peers a)
mkPeers = Peers <$> newTVar [] <*> newTVar [] <*> newTVar []

initPeers :: (Show n, FromJSON n)
             => Node n                       -- ^ our node
             -> ConnectFunction n
             -> FilePath
             -> IO ()
initPeers node connect dataDir = do
  let
    kpFile = dataDir </> "peers"

  void $ forkIO $ maintainConnections node connect
    
  logI $ "reading known peers from " ++ kpFile
  kpbs <- BSL.readFile kpFile
  
  case eitherDecode kpbs of
    Left  e     -> logW ("error parsing peers file: " ++ e)
    Right peers -> do
      logI ("got " ++ (show $ length peers) ++ " peers")
      atomically $ writeTVar (peersKnown $ nodePeers node) peers

addPeer :: Peers a -> PeerNode a -> STM ()
addPeer p n = do
  modifyTVar' (peersConnecting p) $ filter (/= nodePeer n)
  modifyTVar' (peersConnected p) ((:) n)
  
removePeer :: Peers a -> PeerNode a -> STM ()
removePeer p n = do
  modifyTVar' (peersConnecting p) $ filter (/= nodePeer n)
  modifyTVar' (peersConnected p)  $ filter (/= n)
  -- TODO: remove this peer's messages from routing map
  
maintainConnections :: (Show a) => Node a -> ConnectFunction a -> IO ()
maintainConnections node connect = forever $ do
  -- we simply try to maintain a connection to all known peers for now
  delay <- registerDelay $ 2 * 1000 * 1000 -- limit outgoing connection rate
  
  let
    peers = nodePeers node
  
  shouldConnect <- atomically $ do
    readTVar delay >>= check
    
    known <- readTVar $ peersKnown peers
    cting <- readTVar $ peersConnecting peers
    connected <- readTVar $ peersConnected peers
  
    let
      result = (known \\ cting) \\ (map nodePeer connected)

    if null result
      then retry
      else do
        let result' = head result
        modifyTVar (peersConnecting peers) ((:) result')
        return result'

  logI $ "connecting to " ++ show shouldConnect
  void $ forkIO $ connect shouldConnect $ \cresult -> do
    case cresult of
      Left e -> do
        logW $ "error connecting: " ++ e ++ " on " ++ show shouldConnect
        atomically $ modifyTVar (peersConnecting peers) (filter $ (/= shouldConnect))
      
      Right msgio -> do
        logI $ "connected to " ++ show shouldConnect
        runPeerNode node msgio (Just shouldConnect)

-------------------------------------------------------------------------
-- Peer Nodes
-------------------------------------------------------------------------

data PeerNode a
  = PeerNode
    { nodePeer :: Peer a
    , nQueue   :: TBMQueue (Message a) -- ^ outgoing message queue to this node
    }
             
instance (Show a) => Show (PeerNode a) where
  show n = "Node {peer = " ++ show (nodePeer n) ++ " }"

instance Eq (PeerNode a) where
  (PeerNode p1 _) == (PeerNode p2 _) = p1 == p2

-- | puts a message on the Node's outgoing message queue       
enqMessage :: PeerNode a -> Message a -> STM ()
enqMessage n m = writeTBMQueue (nQueue n) m

------------------------------------------------------------------------------
-- Handshake
------------------------------------------------------------------------------

-- ^ handle a node that is currently connected to us
runPeerNode
  :: (Show a)
  => Node a
  -> MessageIO a    -- ^ the (source, sink) pair to talk to the peer
  -> Maybe (Peer a)
  -> IO ()
runPeerNode node (src, sink) expected = do
  mq <- newTBMQueueIO 50
  
  -- enqueue the obligatory Hello message, if this is an
  -- outgoing connection
  when (isJust expected) $ atomically $ writeTBMQueue mq (Direct $ Hello $ nodeIdentity node)
  
  tid <- forkIO $ src C.$$ C.awaitForever $ \msg -> do
    case msg of
      Direct (Hello p) -> do
        case expected of
          Nothing -> return ()
          Just e -> when (e /= p) $ error "node identity mismatch"
            
        let pn = PeerNode p mq
        
        liftIO $ do
          logI $ "got hello from " ++ show pn
          
          atomically $ do
            unless (isJust expected) $ writeTBMQueue mq (Direct $ Hello $ nodeIdentity node)
            addPeer (nodePeers node) pn
            
        C.addCleanup
          (\_ -> do
              logI $ "lost connection to " ++ (show $ peerNodeInfo $ nodePeer pn)
              atomically $ removePeer (nodePeers node) pn)
          (C.mapM_ $ \m -> atomically $ writeTChan (nodeIncoming node) (pn, m))
        
      x       -> error $ show x
        
  C.addCleanup (\_ -> killThread tid) (C.sourceTBMQueue mq) C.$$ sink

instance (Show a) => UriFetch (Node a) where
  getUriData = requestNodeData

requestNodeData :: (Show a) => Node a -> FN.URI -> IO (Either T.Text BSL.ByteString)
requestNodeData n (FN.CHK loc key extra _) = do
  case FN.chkExtraCompression extra of
    Left  e -> return $ Left $ "can't decompress CHK: " `T.append` e
    Right c -> do
      result <- requestChk n $ FN.ChkRequest loc (FN.chkExtraCrypto extra)
  
      case result of
        Left e    -> return $ Left $ "obtaining CHK data block failed: " `T.append` e
        Right blk -> case FN.decryptDataBlock blk key $ FN.chkExtraCrypto extra of
          Left e  -> return $ Left $ "decrypting CHK data block failed: " `T.append` e
          Right p -> FN.decompressChk c p

requestNodeData n (FN.SSK pkh key extra dn _) = do
  result <- requestSsk n $ FN.SskRequest pkh (FN.sskEncryptDocname key dn) (FN.sskExtraCrypto extra)

  return $ case result of
    Left e    -> Left e
    Right blk -> FN.decryptDataBlock blk key $ FN.sskExtraCrypto extra

requestNodeData n (FN.USK pkh key extra dn dr _) = do
  result <- let dn' = dn `T.append` "-" `T.append` (T.pack $ show dr)
            in requestSsk n $ FN.SskRequest pkh (FN.sskEncryptDocname key dn') (FN.sskExtraCrypto extra)

  return $ case result of
    Left e    -> Left e
    Right blk -> FN.decryptDataBlock blk key $ FN.sskExtraCrypto extra
