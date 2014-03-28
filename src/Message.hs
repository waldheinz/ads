
module Message (
  Message(..), MessageIO
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Binary
import Data.Conduit

import NextBestOnce
import Types

-- |
-- A (source, sink) pair of messages, suitable for talking to a node.
type MessageIO a = (Source IO (Message a), Sink (Message a) IO ())

data (Show a) => Message a
     = Hello (Peer a)
     | Ping
     | Routed                          -- ^ a message which should be routed to another peer
       { routedPayload :: Message a
       , routedIfo     :: RoutingInfo NodeId
       }
     deriving ( Show )

putHeader :: Word8 -> Put
putHeader t = put t

instance (Binary a, Show a) => Binary (Message a) where
  put (Hello ni)      = putHeader 1 >> put ni
  put Ping            = putHeader 2
  put (Routed msg ri) = putHeader 3 >> put msg >> put ri
  
  get = do
    t <- get :: Get Word8
    
    case t of
      1 -> Hello <$> get
      2 -> return Ping
      3 -> Routed <$> get <*> get
      _ -> fail $ "unknown message type " ++ show t
