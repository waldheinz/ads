
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Keys (
  CHK, decryptDataFound
  ) where

import Crypto.Cipher.AES
import Data.Binary
import Data.Binary.Get ( getByteString, getWord16be, runGet )
import Data.Binary.Put ( putByteString )
import qualified Data.ByteString as BS
import Data.ByteString.Lazy ( fromStrict )
import Data.Digest.Pure.SHA
import qualified Data.Text as T
import Freenet.Types
import Types

newtype CHK = CHK BS.ByteString

chkPayloadSize :: Int
chkPayloadSize = 32 * 1024

instance Binary CHK where
  put (CHK p) = putByteString p
  get = fmap CHK (getByteString chkPayloadSize)

instance Block CHK where
  size _ = chkPayloadSize
  
decryptDataFound
  :: Key                          -- ^ the secret crypto key (second part of URIs)
  -> DataFound                    -- ^ the encrypted data together with their headers
  -> Either T.Text BS.ByteString  -- ^ the decrypted payload
decryptDataFound key (ChkFound _ header ciphertext) =
  let
    hash = BS.take 32 $ BS.drop 2 header
    cipherLen = BS.drop 34 header -- that's already part of the hash ?????
    iv = BS.take 16 hash
    aes = initAES $ unKey key
    plaintext'' = decryptCTR aes iv $ BS.concat [ciphertext, cipherLen] -- TODO get rid of the concat
    (plaintext', lenbytes) = BS.splitAt (BS.length ciphertext) plaintext''
    len = fromIntegral $ runGet getWord16be $ fromStrict lenbytes
    plaintext = BS.take len plaintext'
    mac = bytestringDigest (hmacSha256 (fromStrict $ unKey key) (fromStrict plaintext''))
    result
      | len > BS.length plaintext = Left $ "invalid length"
      | mac /= fromStrict hash = Left $ "mac mismatch"
      | otherwise = Right plaintext
  in result
     
