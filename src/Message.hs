
{-# LANGUAGE MultiParamTypeClasses #-}

module Message (
  MessagePayload(..), RoutedMessage(..), 
  MessageSource, MessageSink, MessageIO,
  Message(..), MessageId
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Binary
import Data.Conduit

import qualified Freenet.Chk as FN
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
data MessagePayload a
     = Hello (Peer a)
     | Ping
     | FreenetChkRequest FN.ChkRequest
     | FreenetChkBlock   FN.ChkBlock
     deriving ( Show )

instance (Binary a) => Binary (MessagePayload a) where
  put (Hello peer)           = putHeader 1 >> put peer
  put Ping                   = putHeader 2
  put (FreenetChkRequest dr) = putHeader 3 >> put dr
  put (FreenetChkBlock blk)  = putHeader 4 >> put blk
  
  get = do
    t <- getWord8
    
    case t of
      1 -> Hello <$> get
      2 -> return Ping
      3 -> FreenetChkRequest <$> get
      4 -> FreenetChkBlock <$> get
      _ -> fail $ "unknown message type " ++ show t


-- |
-- a message which should be routed to another peer
data Message a = Routed (RoutedMessage a)
               | Response MessageId (MessagePayload a)
               | Direct (MessagePayload a)
               deriving (Show)

instance Binary a => Binary (Message a) where
  put (Routed msg)       = putHeader 1 >> put msg
  put (Response mid msg) = putHeader 2 >> put mid >> put msg
  put (Direct msg)       = putHeader 3 >> put msg
  
  get = do
    t <- getWord8
    case t of
      1 -> Routed <$> get
      2 -> Response <$> get <*> get
      3 -> Direct <$> get
      x -> fail $ "unknown message type " ++ show x

data RoutedMessage a = RoutedMessage
                       { rmPayload :: MessagePayload a
                       , rmId      :: MessageId
                       , rmInfo    :: NBO.RoutingInfo NodeId
                       }
                       deriving ( Show )

instance Binary a => Binary (RoutedMessage a) where
  put (RoutedMessage p mid ri) = put p >> put mid >> put ri
  get = RoutedMessage <$> get <*> get <*> get
  
putHeader :: Word8 -> Put
putHeader t = put t

