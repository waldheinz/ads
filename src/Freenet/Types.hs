
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

module Freenet.Types (
  Key(..), mkKey, mkKey',
  DataRequest(..), DataFound(..), StorePersistable(..)
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

class DataFound f where
  dataFoundLocation :: f -> Key
  decryptDataFound
    :: f      
    -> Key                          -- ^ the secret key
    -> Word8                        -- ^ the crypto algorithm used (why this is not stored with the data is not known)
    -> Either T.Text BSL.ByteString -- ^ either a beefy error message or the decrypted payload, already trimmed to correct length

data DataRequest
     = ChkRequest
       { chkReqLocation :: ! Key   -- ^ the location of the data
       , chkReqHashAlg  :: ! Word8 -- ^ the hash algorithm to use
       }
     | SskRequest
       { sskReqPkh      :: ! Key
       , sskReqEhd      :: ! Key
       , sskReqAlg      :: ! Word8
       }  
       deriving ( Show )

instance Binary DataRequest where
  put (ChkRequest l h) = putWord8 1 >> put l >> put h
  put (SskRequest pk eh a) = putWord8 2 >> put pk >> put eh >> put a

  get = do
    tp <- getWord8

    case tp of
      1 -> ChkRequest <$> get <*> get
      2 -> SskRequest <$> get <*> get <*> get
      x -> fail $ "unknown data request type " ++ show x
      
class DataFound a => StorePersistable a where
  storeSize :: a -> Int
  storePut  :: a -> Put
  storeGet  :: DataRequest -> Get a
