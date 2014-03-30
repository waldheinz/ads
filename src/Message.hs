
{-# LANGUAGE MultiParamTypeClasses #-}

module Message (
  Message(..), MessageIO
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Binary
import Data.Conduit

import qualified Freenet.Messages as FNM
import qualified NextBestOnce as NBO
import Types

-- |
-- A (source, sink) pair of messages, suitable for talking to a node.
type MessageIO a = (Source IO (Message a), Sink (Message a) IO ())

-- |
-- Messages are parametrised over the type of Peer addresses used, which could
-- be either hostnames or message queues for simulations.
data (Show a) => Message a
     = Hello (Peer a)
     | Ping
     | Routed RoutedMessage
     | FreenetMessage FNM.Message
     deriving ( Show )

-- |
-- a message which should be routed to another peer
data RoutedMessage = RM
                       { rmPayload :: Message NodeId
                       , rmInfo    :: NBO.RoutingInfo NodeId
                       }

instance Show RoutedMessage where
  show (RM p i) = "routed to " ++ show i

putHeader :: Word8 -> Put
putHeader t = put t

instance (Binary a, Show a) => Binary (Message a) where
  put (Hello ni)      = putHeader 1 >> put ni
  put Ping            = putHeader 2
  put (Routed (RM msg ri)) = putHeader 3 >> put msg >> put ri
  
  get = do
    t <- get :: Get Word8
    
    case t of
      1 -> Hello <$> get
      2 -> return Ping
      3 -> do
           rm <- RM <$> get <*> get
           return $ Routed rm
      _ -> fail $ "unknown message type " ++ show t
