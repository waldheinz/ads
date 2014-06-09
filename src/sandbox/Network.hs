
module Network (
--  NetInterface, mkNetInterface,
  MsgPassAddress
  ) where

import Data.Conduit
import Data.Conduit.TQueue

import Message
import Peers

type SBMessage = Message NetInterface

data MsgPassAddress = MsgPassAddress deriving ( Show, Eq )

instance PeerAddress MsgPassAddress where
  connectPeer _ k = k $ Left "niConnect not implemented"
  
newtype NetInterface = NetInterface (Source IO SBMessage, Sink SBMessage IO ())

mkNetInterface :: IO NetInterface
mkNetInterface = fmap NetInterface $ entangledPair 3
