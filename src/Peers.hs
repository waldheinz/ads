
{-# LANGUAGE OverloadedStrings #-}

module Peers (
  mkNodeInfo,

  PeerAddress, 
  Peer(..), mkPeer,
  ) where

import Control.Concurrent.STM
import Data.Aeson

import Types

-- |
-- Extracts the persistable node information from a peer.
mkNodeInfo :: Peer a -> STM (NodeInfo a)
mkNodeInfo (Peer pid addrs) = do
  as <- readTVar addrs
  return $ NodeInfo pid as

----------------------------------------------------------------
-- Peers
----------------------------------------------------------------

class (FromJSON a, Show a, Eq a) => PeerAddress a where

-- |
-- This is the live version of @NodeInfo@, which can be constructed
-- from @NodeInfo@ and converted to @NodeInfo@ for storage and transfer.
data Peer a = Peer
            { peerId          :: NodeId       -- ^ the static node info of this peer
            , peerAddresses   :: TVar [a]     -- ^ where this peer can be connected
            }

instance Eq (Peer a) where
  (==) p1 p2 = (peerId p1) == (peerId p2)

instance ToJSON a => ToStateJSON (Peer a) where
  toStateJSON (Peer pid adds) = do
    adds' <- readTVar adds
    
    return $ object
      [ "id"        .= pid
      , "addresses" .= adds'
      ]
  
mkPeer :: PeerAddress a => NodeInfo a -> STM (Peer a)
mkPeer (NodeInfo nid addrs) = do
  as <- newTVar addrs
  return $ Peer nid as

