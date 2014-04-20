
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

module Freenet.Types (
  Key(..), mkKey, mkKey',
  
  DataRequest(..),
  DataBlock(..),
  freenetLocation,
  
  StorePersistable(..)
  ) where

import Control.Applicative ( (<$>) )
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits ( shiftL, testBit, xor )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA ( bytestringDigest, sha256 )
import Data.Hashable
import qualified Data.Text as T

import Freenet.Base64

newtype Key = Key { unKey :: BS.ByteString } deriving ( Eq, Ord )

keySize :: Int
keySize = 32

instance Show Key where
  show (Key bs) = T.unpack $ toBase64' bs

instance Hashable Key where
  hashWithSalt s k = hashWithSalt s (unKey k)

instance Binary Key where
  put (Key k) = putByteString k
  get = Key <$> getByteString keySize

mkKey :: BS.ByteString -> Either T.Text Key
mkKey bs = if BS.length bs == keySize
           then Right $ Key bs
           else Left  $ "keys must be 32 bytes"

mkKey' :: BS.ByteString -> Key
mkKey' bs
  | BS.length bs == keySize = Key bs
  | otherwise               = error "expected 32 bytes in mkKey"

-----------------------------------------------------------------------------------------
-- data blocks, requests and routing them
-----------------------------------------------------------------------------------------

--class FreenetRoutable r where
  
  
-- |
-- A still encrypted, but verified, chunk of data.
class DataBlock f where

  -- |
  -- Use @freenetLocation@ to implement this. Really.
  dataBlockLocation :: f -> Key

  -- |
  -- Gives the type code for the datablock. This is only needed to determine the
  -- @dataBlockRoutingLocation@.
--  dataBlockType :: f -> Word16
  
  -- |
  -- Decrypts the data block. Be aware that the resulting data may still
  -- be compressed. If and how to decompress is specific to the data block.
  decryptDataBlock
    :: f      
    -> Key                                -- ^ the secret key
    -> Either T.Text (BS.ByteString, Int) -- ^ either an error or (decrypted payload, original length)
      
class DataBlock a => StorePersistable a where
  storeSize :: a -> Int
  storePut  :: a -> Put
  storeGet  :: Get a

-- |
-- Gives the true location (aka "routing key") of a data block. When it comes to actually
-- routing data, Freenet does not directly use the routing key stored with the blocks, but
-- does another round of SHA256 using the location and the key type (a Word16) as input.
-- The top 64 bits of the result are then interpreted as an signed log, which gets it's sign bit
-- reset, giving the true routing destination. It's in freenet.keys.Key.toNormalizedDouble().
-- Sigh.
--
-- What this function aims for:
--   * we want locations close in Freenet keyspace to be close in our keyspace as well (and vice versa)
--   * we do not want to lose 203 bits of precision which are caused by
--     Freenet's conversion to a double
freenetLocation
  :: Key          -- ^ the original location stored with the request / block
  -> Word16       -- ^ the "type", see freenet.keys.Key.getType()
  -> Key          -- ^ the modified location
freenetLocation key tp = mkKey' padded where
  digest   = BSL.toStrict $ bytestringDigest $ sha256 $ BSL.fromChunks [unKey key, BSL.toStrict $ encode tp]
  digest'  = let (msb, lsb) = BS.splitAt 8 digest in bsToPosI $ (BS.reverse msb)  `BS.append` lsb
  digest''
    | testBit digest' 255 = (digest' `xor` (0xffffffffffffffff `shiftL` 192)) `shiftL` 1
    | otherwise = digest' `shiftL` 1
  bs       = posIToBs digest''
  padded   = (BS.replicate (32 - BS.length bs) 0) `BS.append` bs

class DataRequest a where
  
  -- |
  -- Use @freenetLocation@ to implement this.
  dataRequestLocation :: a -> Key
  
