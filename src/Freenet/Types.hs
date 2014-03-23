
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Types (
  Key(..), mkKey, mkKey',
  
  -- * things that go to the store
  StorePersistable(..)
  ) where

import Control.Applicative ( (<$>) )
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString as BS
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

----------------------------------------------------------------
-- store related
----------------------------------------------------------------

class StorePersistable a where
  storePersistFile :: a -> String
  storePersistPut  :: a -> Put
  storePersistGet  :: String -> Get a -- ^ given a filename, get the data
  
