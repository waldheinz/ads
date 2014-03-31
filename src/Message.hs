
{-# LANGUAGE MultiParamTypeClasses #-}

module Message (
  Message(..),
  MessageSource, MessageSink, MessageIO,
  RoutedMessage(..)
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Binary
import Data.Conduit

import qualified Freenet.Messages as FNM
import qualified NextBestOnce as NBO
import Types

-- |
-- A source of messages, which usually would be another node
-- talking to us.
type MessageSource a = Source IO (Message a)

-- |
-- A sink for outgoing messages to another node.
type MessageSink a = Sink (Message a) IO ()

-- |
-- A (source, sink) pair of messages, suitable for talking to a node.
type MessageIO a = (MessageSource a, MessageSink a)

type MessageId = Word64

-- |
-- Messages are parametrised over the type of Peer addresses used, which could
-- be either hostnames or message queues for simulations.
data (Show a) => Message a
     = Hello (Peer a)
     | Ping
     | Routed (RoutedMessage a)
     | FreenetMessage FNM.FreenetMessage
     deriving ( Show )

-- |
-- a message which should be routed to another peer
data RoutedMessage a = RoutedMessage
                       { rmPayload :: Message a
                       , rmId      :: MessageId
                       , rmInfo    :: NBO.RoutingInfo NodeId
                       }

instance (Show a) => Show (RoutedMessage a) where
  show (RoutedMessage p _ i) = show p ++ " (routed to " ++ show i ++ ")"

putHeader :: Word8 -> Put
putHeader t = put t

instance (Binary a, Show a) => Binary (Message a) where
  put (Hello ni)                          = putHeader 1 >> put ni
  put Ping                                = putHeader 2
  put (Routed (RoutedMessage msg mid ri)) = putHeader 3 >> put msg >> put mid >> put ri
  put (FreenetMessage fm)                 = putHeader 4 >> put fm
  
  get = do
    t <- get :: Get Word8
    
    case t of
      1 -> Hello <$> get
      2 -> return Ping
      3 -> do
           rm <- RoutedMessage <$> get <*> get <*> get
           return $ Routed rm
      4 -> FreenetMessage <$> get
      _ -> fail $ "unknown message type " ++ show t
