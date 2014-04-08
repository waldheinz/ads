
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
import Data.List ( (\\), find )
import qualified Data.HashMap.Strict as HMap
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
            , nodeArchives :: FN.ArchiveCache
            , nodeChkRequests :: RequestManager FN.ChkRequest FN.ChkBlock
            , nodeSskRequests :: RequestManager FN.SskRequest FN.SskBlock
            }

mkNode :: (Show a) => Peer a -> FN.Freenet a -> IO (Node a)
mkNode self fn = do
  peers  <- atomically $ mkPeers
  midgen <- mkMessageIdGen
  msgMap <- newTVarIO Map.empty
  ac     <- FN.mkArchiveCache 10
  chkRm  <- atomically $ mkRequestManager
  sskRm  <- atomically $ mkRequestManager
  
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

    node = Node peers self midgen msgMap nbo fn ac chkRm sskRm

  return node

handlePeerMessages :: PeerAddress a => Node a -> PeerNode a -> Message a -> IO ()

handlePeerMessages node pn (Direct GetPeerList) =
  atomically $ readTVar (peersKnown $ nodePeers node) >>= \ps -> enqMessage pn $ Direct $ PeerList ps
                                                                 
handlePeerMessages node _  (Direct (PeerList ps)) =
  atomically $ mapM_ (mergePeer node) ps
  
handlePeerMessages node pn msg = do
  let
    fn = nodeFreenet node
    
    route = void $ forkIO $ case msg of
      Routed False rm@(RoutedMessage (FreenetChkRequest req) mid _) -> do
        local <- FN.getChk fn req
        case local of
          Left _    -> sendRoutedMessage node rm (Just pn) -- pass on
          Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetChkBlock blk

      Routed False rm@(RoutedMessage (FreenetSskRequest req) mid _) -> do
        local <- FN.getSsk fn req
        case local of
          Left _    -> sendRoutedMessage node rm (Just pn) -- pass on
          Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetSskBlock blk
          
      Routed True rm    -> sendRoutedMessage node rm Nothing -- backtrack
      Response mid msg' -> forwardResponse node mid msg'

      _ -> return ()

    writeStores = case msg of
      Response _ (FreenetChkBlock blk) -> do
        atomically $ offer blk (nodeChkRequests node)
        FN.offerChk (nodeFreenet node) blk
        
      Response _ (FreenetSskBlock blk) -> do
        atomically $ offer blk (nodeSskRequests node)
        FN.offerSsk (nodeFreenet node) blk
      _   -> return ()
    
  writeStores >> route

-- |
-- Turn a Freenet @Key@ into a @NodeId@ by repacking the 32 bytes.
keyToTarget :: FN.Key -> NodeId
keyToTarget key = mkNodeId' $ FN.unKey key

mkRoutedMessage :: Node a -> NodeId -> MessagePayload a -> IO (RoutedMessage a)
mkRoutedMessage node target msg = atomically $ do
  mid <- nextMessageId $ nodeMidGen node
  return $ RoutedMessage msg mid $ NBO.mkRoutingInfo target

sendRoutedMessage :: Show a => Node a -> RoutedMessage a -> Maybe (PeerNode a) -> IO ()
sendRoutedMessage node msg prev = do
  mlog <- atomically $ do
    result <- NBO.route (nodeNbo node) prev msg
    case result of
      NBO.Forward dest msg'   -> enqMessage dest (Routed False msg') >> return Nothing
      NBO.Backtrack dest msg' -> enqMessage dest (Routed True  msg') >> return Nothing
      NBO.Fail                -> return $ Just $ logI $ "message failed fatally: " ++ show msg
      
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

initPeers :: (PeerAddress a)
             => Node a                       -- ^ our node
             -> ConnectFunction a
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

-- |
-- Merges the information about some peer with our set of known peers,
-- adding new ones or just updating addresses of the ones we already
-- know about.
mergePeer :: PeerAddress a => Node a -> Peer a -> STM ()
mergePeer node p = unless (p == nodeIdentity node) $ do
  let ps = nodePeers node
  known <- readTVar $ peersKnown ps

  let known' = case find (== p) known of
        Nothing -> (p:known)
        Just p' -> (p { peerAddress = mergeAddress (peerAddress p) (peerAddress p') } : filter (==p) known)

  writeTVar (peersKnown ps) (known')

-- |
-- Adds a peer to the set of connected peers. It is now a partner
-- for message exchange, instead of just someone we know of.
addPeerNode :: PeerAddress a => Node a -> PeerNode a -> STM ()
addPeerNode node pn = do
  let ps = nodePeers node
  mergePeer node (nodePeer pn)
  modifyTVar' (peersConnecting ps) $ filter (/= nodePeer pn)
  modifyTVar' (peersConnected ps) ((:) pn)

-- |
-- Removed a peer from the set of connected peers, when we decided
-- we don't want to talk to this peer any more, or the connection
-- was lost.
removePeerNode :: Peers a -> PeerNode a -> STM ()
removePeerNode p n = do
  modifyTVar' (peersConnecting p) $ filter (/= nodePeer n)
  modifyTVar' (peersConnected p)  $ filter (/= n)
  -- TODO: remove this peer's messages from routing map
  
maintainConnections :: PeerAddress a => Node a -> ConnectFunction a -> IO ()
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
  :: (PeerAddress a)
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
            writeTBMQueue mq (Direct GetPeerList)
            addPeerNode node pn
            
        C.addCleanup
          (\_ -> do
              logI $ "lost connection to " ++ (show $ peerNodeInfo $ nodePeer pn)
              atomically $ removePeerNode (nodePeers node) pn)
          (C.mapM_ $ handlePeerMessages node pn)
        
      x       -> error $ show x
        
  C.addCleanup (\_ -> killThread tid) (C.sourceTBMQueue mq) C.$$ sink

instance (Show a) => UriFetch (Node a) where
  getUriData = requestNodeData

requestNodeData :: (Show a) => Node a -> FN.URI -> IO (Either T.Text BSL.ByteString)
requestNodeData n (FN.CHK loc key extra _) = do
  case FN.chkExtraCompression extra of
    Left  e -> return $ Left $ "can't decompress CHK: " `T.append` e
    Right c -> do
      let
        req = FN.ChkRequest loc (FN.chkExtraCrypto extra)
        decrypt blk = case FN.decryptDataBlock blk key $ FN.chkExtraCrypto extra of
          Left e  -> return $ Left $ "decrypting CHK data block failed: " `T.append` e
          Right p -> FN.decompressChk c p
 
      fromStore <- FN.getChk (nodeFreenet n) req

      case fromStore of
        Right blk -> decrypt blk
        Left _    -> do
          d <- request (nodeChkRequests n) req $ \r -> do
            msg <- mkRoutedMessage n (keyToTarget $ FN.dataRequestLocation req) (FreenetChkRequest r)
            sendRoutedMessage n msg Nothing
          
          result <- atomically $ waitDelayed d
          
          case result of
            Nothing  -> return $ Left $ "timeout waiting for CHK data"
            Just blk -> decrypt blk
            
requestNodeData n (FN.SSK pkh key extra dn _) = do
  let
    req = FN.SskRequest pkh (FN.sskEncryptDocname key dn) (FN.sskExtraCrypto extra)
    decrypt blk = FN.decryptDataBlock blk key $ FN.sskExtraCrypto extra
        
  fromStore <- FN.getSsk (nodeFreenet n) req

  case fromStore of
    Right blk -> return $ decrypt blk
    Left _    -> do
      d <- request (nodeSskRequests n) req $ \r -> do
        msg <- mkRoutedMessage n (keyToTarget $ FN.dataRequestLocation req) (FreenetSskRequest r)
        sendRoutedMessage n msg Nothing
          
      result <- atomically $ waitDelayed d
          
      case result of
        Nothing  -> return $ Left $ "timeout waiting for SSK data"
        Just blk -> return $ decrypt blk

requestNodeData n (FN.USK pkh key extra dn dr _) = do
  let
    dn' = dn `T.append` "-" `T.append` (T.pack $ show dr)
    req = FN.SskRequest pkh (FN.sskEncryptDocname key dn') (FN.sskExtraCrypto extra)
    decrypt blk = FN.decryptDataBlock blk key $ FN.sskExtraCrypto extra
        
  fromStore <- FN.getSsk (nodeFreenet n) req

  case fromStore of
    Right blk -> return $ decrypt blk
    Left _    -> do
      d <- request (nodeSskRequests n) req $ \r -> do
        msg <- mkRoutedMessage n (keyToTarget $ FN.dataRequestLocation req) (FreenetSskRequest r)
        sendRoutedMessage n msg Nothing
          
      result <- atomically $ waitDelayed d
          
      case result of
        Nothing  -> return $ Left $ "timeout waiting for SSK data"
        Just blk -> return $ decrypt blk

-------------------------------------------------------------------------------------------------
-- Organizing data requests
-------------------------------------------------------------------------------------------------

data Delayed d = Delayed ! (TMVar (Maybe d))

waitDelayed :: Delayed d -> STM (Maybe d)
waitDelayed (Delayed d) = readTMVar d

data RequestManager r d = RequestManager
                        { rmRequests :: TVar (HMap.HashMap FN.Key (Delayed d))
                        , rmTimeout  :: Int
                        }

mkRequestManager :: STM (RequestManager r d)
mkRequestManager = do
  reqs <- newTVar HMap.empty
  return $! RequestManager reqs (20 * 1000 * 1000)

offer :: (FN.DataBlock d) => d -> RequestManager r d -> STM ()
offer db rmgr = do

  let
    key = FN.dataBlockLocation db
  
  rm <- readTVar (rmRequests rmgr)

  case HMap.lookup key rm of
    Nothing          -> return ()
    Just (Delayed d) -> do
      putTMVar d (Just db)
      writeTVar (rmRequests rmgr) $ HMap.delete key rm

request :: (FN.DataRequest r) => RequestManager r d -> r -> (r -> IO ()) -> IO (Delayed d)
request rmgr dr act = do
  let
    key = FN.dataRequestLocation dr
    checkTimeout (Delayed d) to = orElse
      (isEmptyTMVar d >>= \e -> if e then retry else return ())
      (readTVar to    >>= \t -> if t
                                then putTMVar d Nothing >> modifyTVar' (rmRequests rmgr) (HMap.delete key)
                                else retry)
                   
  (result, needStart) <- atomically $ do
    rm <- readTVar (rmRequests rmgr)
    case HMap.lookup key rm of
      Just old -> return (old, False)   -- request is already running
      Nothing  -> do                    -- make new Delayed
        b <- newEmptyTMVar
        let d = Delayed b
        writeTVar (rmRequests rmgr) $ HMap.insert key d rm
        return (d, True)
  
  when needStart $ do
    to <- registerDelay $ rmTimeout rmgr
    void $ forkIO $ atomically $ checkTimeout result to
    act dr
    
  return result
  
