
{-# LANGUAGE MultiParamTypeClasses #-}

module Message (
  MessagePayload(..), RoutedMessage(..), 
  MessageSource, MessageSink, MessageIO,
  Message(..),
  -- * Message IDs
  MessageId, MessageIdGen, mkMessageIdGen, nextMessageId
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void )
import qualified Crypto.Random.AESCtr as AESRNG
import Data.Binary
import Data.Conduit
import System.Random ( random )

import qualified Freenet.Chk as FN
import qualified Freenet.Ssk as FN
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

-------------------------------------------------------------------------------------
-- Message IDs
-------------------------------------------------------------------------------------

type MessageId = Word64

newtype MessageIdGen = MessageIdGen { unMessageIdGen :: TBQueue Word64  }

mkMessageIdGen :: IO MessageIdGen
mkMessageIdGen = do
  q   <- newTBQueueIO 64
  rng <- AESRNG.makeSystem

  let makeId r = let (next, r') = random r
                 in do
                   atomically $ writeTBQueue q next
                   makeId r'
  
  void $ forkIO $ makeId rng
  return $ MessageIdGen q

nextMessageId :: MessageIdGen -> STM MessageId
nextMessageId = readTBQueue . unMessageIdGen

-------------------------------------------------------------------------------------
-- Message Payload
-------------------------------------------------------------------------------------
  
-- |
-- Messages are parametrised over the type of Peer addresses used, which could
-- be either hostnames or message queues for simulations.
data MessagePayload a
     = Hello (NodeInfo a)
     | Ping
     | GetPeerList                      -- ^ request for getting some peers which we might connect to
     | PeerList [NodeInfo a]            -- ^ response to @GetPeers@ request
     | FreenetChkRequest FN.ChkRequest
     | FreenetChkBlock   FN.ChkBlock
     | FreenetSskRequest FN.SskRequest
     | FreenetSskBlock   FN.SskBlock
     | Bye String
     | Failed (Maybe String)            -- ^ 
     deriving ( Show )

instance (Binary a) => Binary (MessagePayload a) where
  put (Hello peer)           = putHeader  1 >> put peer
  put Ping                   = putHeader  2
  put GetPeerList            = putHeader  3
  put (PeerList ps)          = putHeader  4 >> put ps
  put (FreenetChkRequest dr) = putHeader  5 >> put dr
  put (FreenetChkBlock blk)  = putHeader  6 >> put blk
  put (FreenetSskRequest dr) = putHeader  7 >> put dr
  put (FreenetSskBlock blk)  = putHeader  8 >> put blk
  put (Bye msg)              = putHeader  9 >> put msg
  put (Failed reason)        = putHeader 10 >> put reason
  
  get = do
    t <- getWord8
    
    case t of
      1  -> Hello <$> get
      2  -> return Ping
      3  -> return GetPeerList
      4  -> PeerList <$> get
      5  -> FreenetChkRequest <$> get
      6  -> FreenetChkBlock <$> get
      7  -> FreenetSskRequest <$> get
      8  -> FreenetSskBlock <$> get
      9  -> Bye <$> get
      10 -> Failed <$> get
      _  -> fail $ "unknown message type " ++ show t

-- |
-- a message which should be routed to another peer
data Message a = Routed Bool (RoutedMessage a)        -- ^ is this a backtrack step? and the routed message
               | Response MessageId (MessagePayload a)
               | Direct (MessagePayload a)
               deriving (Show)

instance Binary a => Binary (Message a) where
  put (Routed False msg) = putHeader 1 >> put msg
  put (Routed True  msg) = putHeader 2 >> put msg
  put (Response mid msg) = putHeader 3 >> put mid >> put msg
  put (Direct msg)       = putHeader 4 >> put msg
  
  get = do
    t <- getWord8
    case t of
      1 -> Routed False <$> get
      2 -> Routed True  <$> get
      3 -> Response <$> get <*> get
      4 -> Direct <$> get
      x -> fail $ "unknown message type " ++ show x

data RoutedMessage a = RoutedMessage
                       { rmPayload :: MessagePayload a
                       , rmId      :: MessageId
                       , rmMarked  :: [Id]
                       , rmTarget  :: Id
                       }
                       deriving ( Show )

instance Binary a => Binary (RoutedMessage a) where
  put (RoutedMessage p mid ms tgt) = put p >> put mid >> put ms >> put tgt
  get = RoutedMessage <$> get <*> get <*> get <*> get

instance NBO.Routable (RoutedMessage a) Id where
  target = rmTarget
  marked rm l = l `elem` (rmMarked rm)
  mark rm l
    | l `elem` (rmMarked rm) = rm
    | otherwise = rm { rmMarked = (l : (rmMarked rm)) }
  
putHeader :: Word8 -> Put
putHeader t = put t

