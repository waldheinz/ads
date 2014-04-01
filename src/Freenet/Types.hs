
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

module Freenet.Types (
  Key(..), mkKey, mkKey',
  DataRequest(..), DataBlock(..), StorePersistable(..)
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
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

-- |
-- A still encrypted, but verified, chunk of data.
class DataBlock f where
  dataBlockLocation :: f -> Key
  decryptDataBlock
    :: f      
    -> Key                          -- ^ the secret key
    -> Word8                        -- ^ the crypto algorithm used (why this is not stored with the data is not known)
    -> Either T.Text BSL.ByteString -- ^ either a beefy error message or the decrypted payload, already trimmed to correct length
      
class DataBlock a => StorePersistable a where
  storeSize :: a -> Int
  storePut  :: a -> Put
  storeGet  :: Get a

class DataRequest a where
  dataRequestLocation :: a -> Key
  
