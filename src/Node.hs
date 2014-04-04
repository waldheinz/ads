
{-# LANGUAGE OverloadedStrings #-}

module Node (
  -- * our node
  Node, mkNode, requestChk, requestSsk,
  
  -- * peers
  ConnectFunction, initPeers,
  
  -- * other nodes we're connected to
  PeerNode, runPeerNode  
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent ( forkIO, killThread, myThreadId )
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
import Data.Word
import System.FilePath ( (</>) )
import System.Log.Logger

import qualified Freenet as FN
import qualified Freenet.Chk as FN
import qualified Freenet.Ssk as FN
import qualified Freenet.Types as FN
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
            , nextMsgId    :: TVar Word64
            , nodeActMsgs  :: TVar (Map.Map MessageId (PeerNode a, MessagePayload a, NBO.RoutingInfo NodeId)) -- ^ messages we're currently routing
            , nodeNbo      :: NBO.Node NodeId (RoutedMessage a) (PeerNode a)      -- ^ our NBO identity for routing
            , nodeFreenet  :: FN.Freenet a                                        -- ^ our freenet compatibility layer
            , nodeIncoming :: TChan (PeerNode a, Message a)                       -- ^ stream of incoming messages
            }

mkNode :: (Show a) => Peer a -> FN.Freenet a -> IO (Node a)
mkNode self fn = do
  (peers, nmid) <- atomically $ do
    ps <- mkPeers
    mid <- newTVar 0
    return (ps, mid)

  msgMap <- newTVarIO Map.empty
  incoming <- newBroadcastTChanIO
  
  let
    fst' (a, _, _) = a
    nbo = NBO.Node
        { NBO.location          = nodeId $ peerNodeInfo self
        , NBO.neighbours        = readTVar $ peersConnected peers
        , NBO.neighbourLocation = \np -> nodeId $ peerNodeInfo $ nodePeer np
        , NBO.popPred           = \msg -> readTVar msgMap >>= (\m -> return $ maybe Nothing (Just . fst') (Map.lookup (rmId msg) m))
        , NBO.routingInfo       = rmInfo
        , NBO.updateRoutingInfo = \rm ri -> rm { rmInfo = ri }
        }
        
  return $ Node peers self nmid msgMap nbo fn incoming

waitResponse :: Node a -> MessageId -> IO (TMVar (Maybe (MessagePayload a)))
waitResponse node mid = do
  timeout <- registerDelay (30 * 1000 * 1000)
  bucket  <- newEmptyTMVarIO
  chan    <- atomically $ dupTChan (nodeIncoming node)

  let
    doWait = orElse
      (readTChan chan   >>= checkResponse)
      (readTVar timeout >>= \to -> if to then putTMVar bucket Nothing else doWait)
    
    checkResponse msg = case msg of
      (_, (Response mid' rp)) -> if mid == mid' then (putTMVar bucket $ Just rp) else doWait
      _                       -> doWait

  -- wait for data or timeout
  void $ forkIO $ atomically $ doWait

--    forever $ orElse
--    (readTChan chan   >>= checkResponse)
--    (readTVar timeout >>= \to -> if to then putTMVar bucket Nothing else retry)

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
  tid <- myThreadId
  print ("fn", tid, req)
  local <- FN.getChk (nodeFreenet n) req
  case local of
    Right blk -> return $ Right blk
    Left e    -> do
      logI $ "could not fetch data locally " ++ show e
      print ("-> mkr", tid)
      msg <- mkRoutedMessage n (keyToTarget $ FN.dataRequestLocation req) (FreenetChkRequest req)
      print ("-> wr", tid)
      bucket <- waitResponse n $ rmId msg
      print ("-> srm", tid)
      sendRoutedMessage n msg
      print ("-> take", tid)
      result <- atomically $ takeTMVar bucket
      case result of
        Nothing   -> return $ Left "timeout waiting for response"
        Just resp -> case resp of
          (FreenetChkBlock blk) -> return $ Right blk
          x                     -> return $ Left $ "expected CHK block, but got: " `T.append` (T.pack $ show x)

requestSsk :: (Show a) => Node a -> FN.SskRequest -> IO (Either T.Text FN.SskBlock)
requestSsk n req = do
  tid <- myThreadId
  print ("sskfn", tid, req)
  local <- FN.getSsk (nodeFreenet n) req
  case local of
    Right blk -> return $ Right blk
    Left e    -> do
--      logI $ "could not fetch data locally " ++ show e
      print ("-> sskmkr", tid)
      msg <- mkRoutedMessage n (keyToTarget $ FN.dataRequestLocation req) (FreenetSskRequest req)
      print ("-> sskwr", tid)
      bucket <- waitResponse n $ rmId msg
      print ("-> ssksrm", tid)
      sendRoutedMessage n msg
      print ("-> ssktake", tid)
      result <- atomically $ takeTMVar bucket
      case result of
        Nothing   -> return $ Left "timeout waiting for response"
        Just resp -> case resp of
          (FreenetSskBlock blk) -> return $ Right blk
          x                     -> return $ Left $ "expected SSK block, but got: " `T.append` (T.pack $ show x)

mkRoutedMessage :: Node a -> NodeId -> MessagePayload a -> IO (RoutedMessage a)
mkRoutedMessage node target msg = atomically $ do
  mid <- readTVar (nextMsgId node)
  modifyTVar (nextMsgId node) (+1)
  return $ RoutedMessage msg mid $ NBO.mkRoutingInfo target

sendRoutedMessage :: Show a => Node a -> RoutedMessage a -> IO ()
sendRoutedMessage node msg = myThreadId >>= \tid -> do
  mlog <- atomically $ do
    result <- NBO.route (nodeNbo node) Nothing msg
    case result of
      NBO.Forward dest msg -> enqMessage dest (Routed msg) >> (return $ Just $ print ("forwarded", tid))
      NBO.Fail             -> return $ Just $ logI $ "message failed fatally: " ++ show msg

  case mlog of
    Nothing -> return ()
    Just l  -> l
  
sendResponse :: Node a -> MessageId -> MessagePayload a -> IO ()
sendResponse node mid msg = do
  mtgt <- atomically $ do
    m <- readTVar $ nodeActMsgs node
    case Map.lookup mid m of
      Nothing -> return Nothing
      Just t  -> return $ Just t

  case mtgt of
    Nothing  -> logW $ "could not send response, message id unknown: " ++ show mid
    Just (pn, _, _) -> do
      atomically $ enqMessage pn $ Response mid msg

handlePeerMessage :: Show a => Node a -> PeerNode a -> Message a -> IO ()
handlePeerMessage node pn m@(Response mid msg) = do
  atomically $ writeTChan (nodeIncoming node) (pn, m)
--  logI $ "got a reply: " ++ show m
handlePeerMessage node pn (Routed rm@(RoutedMessage rmsg mid ri)) = void $ forkIO $ do
  -- record routing state
--  atomically $ modifyTVar (nodeActMsgs node) $ \m -> Map.insert mid (pn, rmsg, ri) m
  case rmsg of
    FreenetChkRequest req -> do
      local <- FN.getChk (nodeFreenet node) req
      case local of
        Left _    -> sendRoutedMessage node rm -- pass on
        Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetChkBlock blk

    FreenetSskRequest req -> do
      local <- FN.getSsk (nodeFreenet node) req
      case local of
        Left _    -> sendRoutedMessage node rm
        Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetSskBlock blk
      
type ConnectFunction a = Peer a -> ((Either String (MessageIO a)) -> IO ()) -> IO ()

----------------------------------------------------------------
-- the peers list
----------------------------------------------------------------

data Peers a = Peers
               { peersConnected     :: TVar [PeerNode a]      -- ^ peers we're currently connected to
               , peersConnecting    :: TVar [Peer a]          -- ^ peers we're currently trying to connect to
               , peersKnown         :: TVar [Peer a]          -- ^ all peers we know about, includes above
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
          (C.mapM_ $ \msg -> print ("incoming", msg) >> handlePeerMessage node pn msg)
        
      x       -> error $ show x
        
  C.addCleanup (\_ -> killThread tid) (C.sourceTBMQueue mq) C.$$ sink
