
module Peers (
  Peers, mkPeers, addPeer
  ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent.STM

import Node as N

data Peers = Peers
             { peerList :: TVar [N.Node]
             }

mkPeers :: STM Peers
mkPeers = Peers <$> newTVar []

addPeer :: Peers -> N.Node -> STM ()
addPeer p n = modifyTVar' (peerList p) ((:) n)
