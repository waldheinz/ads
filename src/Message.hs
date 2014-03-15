
module Message (
  Message(..)
  ) where

import Control.Applicative ( (<$>) )
import Data.Binary

import Types

data Message
     = Hello NodeInfo
     | Ping
     deriving ( Show )
{-
  Everything is big-endian.

  The basic message header is:
    * 1 byte for the message type
-}

putHeader :: Word8 -> Put
putHeader t = put t
{-
putPut :: (Binary a) => Word8 -> a -> Put
putPut t p
  | len > 10000 = error "too large message for putPut"
  | otherwise = putHeader t (fromIntegral len) >> putLazyByteString bs
  where
    bs = encode p
    len = BSL.length bs
-}
instance Binary Message where
  put (Hello ni) = putHeader 1 >> put ni
  put Ping = putHeader 2

  get = do
    t <- get :: Get Word8
    
    case t of
      1 -> Hello <$> get
      2 -> return Ping
      _ -> fail $ "unknown message type " ++ show t
