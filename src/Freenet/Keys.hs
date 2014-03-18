
module Freenet.Keys (
  CHK
  ) where

import Data.Binary
import Data.Binary.Get ( getByteString )
import Data.Binary.Put ( putByteString )
import qualified Data.ByteString as BS

import Types



newtype CHK = CHK BS.ByteString

chkPayloadSize :: Int
chkPayloadSize = 32 * 1024

instance Binary CHK where
  put (CHK p) = putByteString p
  get = fmap CHK (getByteString chkPayloadSize)

instance Block CHK where
  size _ = chkPayloadSize
  
