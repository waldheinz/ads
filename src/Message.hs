
module Message (
  Message(..)
  ) where

import Control.Applicative ( (<$>) )
import Data.Binary
import Data.Binary.IEEE754
import Data.Binary.Put ( putLazyByteString, runPut )
import qualified Data.ByteString.Lazy as BSL

import Debug.Trace

import Types

data Message
     = Hello NodeInfo
     | Ping
     deriving ( Show )
{-
  Everything is big-endian.

  The basic message header is:
    * 1 byte for the message type
    * 1 uint32 for the remaining message payload length, excluding the header
-}

putHeader :: Word8 -> Word32 -> Put
putHeader t s = put t >> put s

putPut :: (Binary a) => Word8 -> a -> Put
putPut t p
  | len > 10000 = error "too large message for putPut"
  | otherwise = putHeader t (fromIntegral len) >> putLazyByteString bs
  where
    bs = encode p
    len = BSL.length bs

instance Binary Message where
  put (Hello ni) = putPut 1 (ni)
  put Ping = putHeader 2 0

  get = do
    t <- get :: Get Word8
    s <- get :: Get Word32
    
    traceShow t $ case t of
      1 -> Hello <$> get
      2 -> return Ping
      _ -> fail $ "unknown message type " ++ show t
