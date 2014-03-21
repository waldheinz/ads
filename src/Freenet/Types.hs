
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Types (
  Key(..), mkKey, mkKey',
  
  DataRequest(..)
  ) where

import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.Text as T
import Data.Word ( Word8 )

import Freenet.Base64

newtype Key = Key { unKey :: BS.ByteString } deriving ( Eq )

instance Show Key where
  show (Key bs) = T.unpack $ toBase64' bs

instance Hashable Key where
  hashWithSalt s k = hashWithSalt s (unKey k)
  
mkKey :: BS.ByteString -> Either T.Text Key
mkKey bs = if BS.length bs == 32
           then Right $ Key bs
           else Left  $ "keys must be 32 bytes"

mkKey' :: BS.ByteString -> Key
mkKey' bs
  | BS.length bs == 32 = Key bs
  | otherwise          = error "expected 32 bytes in mkKey"

data DataRequest
   = ChkRequest Key Word8 -- ^ the location and the hash algorithm so it can be verified
   deriving ( Show )

