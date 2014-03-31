
{-# LANGUAGE OverloadedStrings #-}

module Node (
  -- * our node
  Node, mkNode,
  handlePeerMessage, sendRoutedMessage,
  nodeSetFreenet,
  
  -- * Peers
  ConnectFunction,
  Peers, mkPeers, initPeers,--  addPeer,
  
  -- * other nodes we're connected to
  PeerNode, nodePeer, enqMessage,
  
  -- * incoming / outgoig messages
  runPeerNode
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBMQueue as STM
import Control.Monad ( forever, unless, void, when )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.TQueue as C
import Data.Aeson
import Data.List ( (\\) )
import qualified Data.Map.Strict as Map
import Data.Word
import System.FilePath ( (</>) )
import System.Log.Logger

import qualified Freenet.Messages as FMSG
import qualified Freenet as FN
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
            , nodeActMsgs  :: TVar (Map.Map Word64 (PeerNode a, RoutedMessage a)) -- ^ messages we're currently routing
            , nodeNbo      :: NBO.Node NodeId (RoutedMessage a) (PeerNode a)      -- ^ our NBO identity for routing
            , nodeFreenet  :: TVar (Maybe (FN.Freenet a))                         -- ^ our freenet compatibility layer
            }

mkNode :: (Show a) => Peer a -> IO (Node a)
mkNode self = do
  (peers, nmid) <- atomically $ do
    ps <- mkPeers
    mid <- newTVar 0
    return (ps, mid)

  msgMap <- newTVarIO Map.empty
  fn     <- newTVarIO Nothing
    
  let nbo = NBO.Node
        { NBO.location          = nodeId $ peerNodeInfo self
        , NBO.neighbours        = readTVar $ peersConnected peers
        , NBO.neighbourLocation = \np -> nodeId $ peerNodeInfo $ nodePeer np
        , NBO.popPred           = \msg -> readTVar msgMap >>= (\m -> return $ maybe Nothing (Just . fst) (Map.lookup (rmId msg) m))
        , NBO.routingInfo       = rmInfo
        , NBO.updateRoutingInfo = \rm ri -> rm { rmInfo = ri }
        }
        
  return $ Node peers self nmid msgMap nbo fn

nodeSetFreenet :: Node a -> FN.Freenet a -> STM ()
nodeSetFreenet n fn = writeTVar (nodeFreenet n) (Just fn)

sendRoutedMessage :: (Show a) => Node a -> NodeId -> Message a -> IO ()
sendRoutedMessage node target msg = do
  result <- atomically $ do
    mid <- readTVar (nextMsgId node)
    modifyTVar (nextMsgId node) (+1)
    NBO.route (nodeNbo node) Nothing (RoutedMessage msg mid $ NBO.mkRoutingInfo target)

  case result of
    NBO.Forward dest msg -> do
      print ("forwarding to", dest)
      atomically $ enqMessage dest $ Routed msg
    x -> do
      print ("unhandled routing result", x)
      
  print result
  return ()

handlePeerMessage :: (Show a) => Node a -> PeerNode a -> Message a -> IO ()
handlePeerMessage node pn msg = case msg of
  Routed rm@(RoutedMessage rmsg mid ri) -> do
    -- record routing state
    atomically $ modifyTVar (nodeActMsgs node) $ \m -> Map.insert mid (pn, rm) m
    case rmsg of
      FreenetMessage fnmsg -> do
        mfn <- atomically $ readTVar (nodeFreenet node)
        case mfn of
          Nothing -> logW "got Freenet message but have no Freenet subsystem"
          Just fn -> FN.handleMessage fn fnmsg
        
      x -> print ("unhandled routed message", rmsg)
      
  x -> print ("unhandled incoming message", msg)

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
addPeer p n = modifyTVar' (peersConnected p) ((:) n)

maintainConnections :: (Show a) => Node a -> ConnectFunction a -> IO ()
maintainConnections node connect = forever $ do
  -- we simply try to maintain a connection to all known peers for now

  let
    peers = nodePeers node
  
  shouldConnect <- atomically $ do
    known <- readTVar $ peersKnown peers
    cting <- readTVar $ peersConnecting peers
    connected <- readTVar $ peersConnected peers
  
    let
      result = known \\ cting
      result' = result \\ (map nodePeer connected)

    if null result'
      then retry
      else do
        let result'' = head result'
        modifyTVar (peersConnecting peers) ((:) result'')
        return result''

  logI $ "connecting to " ++ show shouldConnect
  connect shouldConnect $ \cresult -> do
    case cresult of
      Left e -> do
        logW $ "error connecting: " ++ e ++ " on " ++ show shouldConnect
        atomically $ modifyTVar (peersConnecting peers) (filter ((==) shouldConnect))
      Right msgio -> do
        logI "connected!"
        runPeerNode node msgio True
  
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
  -> MessageIO a -- ^ the (source, sink) pair to talk to the peer
  -> Bool        -- ^ if this is an outbound connection
  -> IO ()
runPeerNode node (src, sink) outbound = do
  mq <- newTBMQueueIO 5
  
  -- enqueue the obligatory Hello message, if this is an
  -- outgoing connection
  when outbound $ atomically $ writeTBMQueue mq (Hello $ nodeIdentity node)

  void $ forkIO $ src C.$$ C.awaitForever $ \msg -> do
    case msg of
      Hello p -> do
        let pn = PeerNode p mq
        
        liftIO $ do
          logI $ "got hello from " ++ show pn
          atomically $ do
            unless outbound $ writeTBMQueue mq (Hello $ nodeIdentity node) 
            addPeer (nodePeers node) pn
          
        C.mapM_ (handlePeerMessage node pn)
        
      x       -> error $ show x
        
  C.sourceTBMQueue mq C.$$ sink
