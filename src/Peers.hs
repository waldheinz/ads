
module Peers (
  mkNodeInfo,

  PeerAddress, 
  Peer(..), mkPeer,
  ) where

import Control.Concurrent.STM
import Data.Aeson

import Types

mkNodeInfo :: Peer a -> STM (NodeInfo a)
mkNodeInfo (Peer pid addrs) = do
  as <- readTVar addrs
  return $ NodeInfo pid as

----------------------------------------------------------------
-- Peers
----------------------------------------------------------------
{-
data PeerConnectState a
  = Disconnected                               -- ^ the peer is currently disconnected
  | Connecting ThreadId                        -- ^ we're currently trying to connect to this peer
  | Connected ThreadId (TBMQueue (Message a))  -- ^ we're connected to the peer
-}

class (FromJSON a, Show a, Eq a) => PeerAddress a where

-- |
-- This is the live version of @NodeInfo@, which can be constructed
-- from @NodeInfo@ and converted to @NodeInfo@ for storage and transfer.
data Peer a = Peer
            { peerId        :: NodeId   -- ^ the static node info of this peer
            , peerAddresses :: TVar [a] -- ^ where this peer can be connected
--            , peerConnState :: TVar (PeerConnectState a)
            }

instance Eq (Peer a) where
  (==) p1 p2 = (peerId p1) == (peerId p2)

instance ToJSON a => ToStateJSON (Peer a) where
  toStateJSON (Peer id adds) = do
    return $ object []
  
mkPeer :: PeerAddress a => NodeInfo a -> STM (Peer a)
mkPeer (NodeInfo nid addrs) = do
  as <- newTVar addrs
--  cs <- newTVar Disconnected
  return $ Peer nid as

