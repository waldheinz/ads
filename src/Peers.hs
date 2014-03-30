
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Maintain connections to other peers.
module Peers (
  ConnectFunction,
  Peers, initPeers, addPeer,
  
  -- * other nodes we're connected to
  PeerNode, nodePeer, enqMessage,
  
  -- * incoming / outgoig messages
  runPeerNode
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.TQueue as C
import Control.Monad ( forever, void )
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.List ( (\\) )
import System.FilePath ( (</>) )
import System.Log.Logger

import Message
--import Node as N
import Types

type ConnectFunction a = Peer a -> ((Either String (MessageIO a)) -> IO ()) -> IO ()

----------------------------------------------------------------
-- the peers list
----------------------------------------------------------------

data Peers a = Peers
             { peersConnected  :: TVar [PeerNode a] -- ^ peers we're currently connected to
             , peersConnecting :: TVar [Peer a]     -- ^ peers we're currently trying to connect to
             , peersKnown      :: TVar [Peer a]     -- ^ all peers we know about, includes above
             }

logI :: String -> IO ()
logI m = infoM "peers" m

logW :: String -> IO ()
logW m = warningM "peers" m

initPeers :: (Show n, FromJSON n) => Peer n -> ConnectFunction n -> FilePath -> IO (Peers n)
initPeers identity connect dataDir = do
  let
    kpFile = dataDir </> "peers"
    
  logI $ "reading known peers from " ++ kpFile
  kpbs <- BSL.readFile kpFile
  
  pk <- case eitherDecode kpbs of
    Left  e     -> logW ("error parsing peers file: " ++ e) >> return []
    Right peers -> logI ("got " ++ (show $ length peers) ++ " peers") >> return peers

  ps <- atomically $ Peers <$> newTVar [] <*> newTVar [] <*> newTVar pk
  void $ forkIO $ maintainConnections identity connect ps
  return ps

addPeer :: Peers a -> PeerNode a -> STM ()
addPeer p n = modifyTVar' (peersConnected p) ((:) n)

maintainConnections :: (Show a) => Peer a -> ConnectFunction a -> Peers a -> IO ()
maintainConnections identity connect peers = forever $ do
  -- we simply try to maintain a connection to all known peers for now

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
        runPeerNode msgio (Just identity) $ \node -> do
          print node
  
  
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
  => MessageIO a            -- ^ the (source, sink) pair to talk to the peer
  -> Maybe (Peer a)         -- ^ our NodeInfo, if this is an outbound connection
  -> (PeerNode a -> IO ())  -- ^ act upon the node once handshake has completed
  -> IO ()
runPeerNode (src, sink) mni connected = do
  mq <- newTBMQueueIO 5
  
  -- enqueue the obligatory Hello message, if this is an
  -- outgoing connection
  case mni of
    Nothing -> return ()
    Just ni -> atomically $ writeTBMQueue mq (Hello ni)

  void $ forkIO $ src C.$$ (C.mapM_ $ \msg -> do
    case msg of
      Hello p -> connected $ PeerNode p mq
      x       -> print ("unhandled message", x)
                                )
  C.sourceTBMQueue mq C.$$ sink
