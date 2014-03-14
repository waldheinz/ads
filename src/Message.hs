
module Message (
  Message(..)
  ) where

import Data.Binary

data Message
     = Ping
     deriving ( Show )
{-
  Everything is big-endian.

  The basic message header is:
    * 1 byte for the message type
    * 1 uint32 for the remaining message payload length, excluding the header
-}

putHeader :: Word8 -> Word32 -> Put
putHeader t s = put t >> put s

instance Binary Message where
  put Ping = putHeader 0 0

  get = do
    t <- get :: Get Word8
    s <- get :: Get Word32
    
    case t of
      0 -> return Ping
      _ -> fail $ "unknown message type " ++ show t
