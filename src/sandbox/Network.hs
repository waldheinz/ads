
module Network (
  MsgPassNet, mkMsgPassNet,
  
  MsgPassAddress, mkAddress
  ) where

import           Control.Concurrent ( forkIO )
import           Control.Concurrent.STM
import           Control.Monad ( liftM2, void )
import           Data.Conduit
import           Data.Conduit.TQueue
import qualified Data.HashMap.Strict as HMap
import           Data.IORef

import           Message
import           Node
import           Peers

type SBMessage = Message MsgPassAddress
type MpIO = (Source IO SBMessage, Sink SBMessage IO ())

data MsgPassAddress = Addr
                      { aId   :: ! Int
                      , aNet  :: ! MsgPassNet
                      , aNode :: ! (Node MsgPassAddress)
                      }

instance Eq MsgPassAddress where
  (==) a1 a2 = aId a1 == aId a2

instance Show MsgPassAddress where
  show a = "MsgPassAddress { id = " ++ (show $ aId a) ++ " }"

instance PeerAddress MsgPassAddress where
  connectPeer p k = do
    addrs <- atomically . readTVar $ peerAddresses p
    
    case addrs of
      []      -> k $ Left "no addresses on peer"
      (a : _) -> do
        (nsrc, psink) <- entangledPair 2
        (psrc, nsink) <- entangledPair 2
        void $ forkIO $ peerConnecting (aNode a) (psrc, psink)
        k $ Right (nsrc, nsink)

data MsgPassNet = MsgPassNet
                  { msgPassAddrs  :: IORef (HMap.HashMap Int (IO MpIO))
                  , msgPassNextId :: IORef Int
                  }

mkMsgPassNet :: IO MsgPassNet
mkMsgPassNet = liftM2 MsgPassNet (newIORef HMap.empty) (newIORef 0)

mkAddress :: Node MsgPassAddress -> MsgPassNet -> IO MsgPassAddress
mkAddress node net = do
  aid <- atomicModifyIORef' (msgPassNextId net) $ \oid -> (oid + 1, oid)
  return $! Addr aid net node
