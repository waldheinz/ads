
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Chk (
  -- * Working with CHKs
  ChkFound(..), mkChkFound, chkPersist,
  decryptChk,

  -- * Requesting CHKs
  ChkRequest(..),
  
  -- * CHK Headers
  ChkHeader, mkChkHeader, unChkHeader,
  chkHeaderHash, chkHeaderCipherLen
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Crypto.Cipher.AES
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put ( putByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA
import qualified Data.Text as T

import Freenet.Base64
import Freenet.Store
import Freenet.Types

------------------------------------------------------------------------------
-- CHK headers
------------------------------------------------------------------------------

-- | the header required to verify a CHK data block
newtype ChkHeader = ChkHeader { unChkHeader :: BS.ByteString } deriving ( Eq )

chkHeaderSize :: Int
chkHeaderSize = 36

instance Show ChkHeader where
  show (ChkHeader bs) = T.unpack $ toBase64' bs

instance Binary ChkHeader where
  put (ChkHeader h) = putByteString h
  get = ChkHeader <$> getByteString chkHeaderSize

mkChkHeader :: BS.ByteString -> Either T.Text ChkHeader
mkChkHeader bs
  | BS.length bs == chkHeaderSize = Right $ ChkHeader bs
  | otherwise = Left $ "CHK header length must be 36 bytes"

chkHeaderHash :: ChkHeader -> BS.ByteString
chkHeaderHash = BS.take 32 . BS.drop 2 . unChkHeader

-- |
-- the length of the CHK plaintext is stored encrypted
-- in it's header, and this function extracts it. yup.
chkHeaderCipherLen :: ChkHeader -> BS.ByteString
chkHeaderCipherLen = BS.drop 34 . unChkHeader

data ChkRequest = ChkRequest Key Word8     -- ^ the location and the hash algorithm so it can be verified
   deriving ( Show )

instance DataRequest ChkRequest where
  dataRequestLocation (ChkRequest k _) = k

data ChkFound = ChkFound !Key !ChkHeader !BS.ByteString -- location, headers and data

instance Show ChkFound where
  show (ChkFound k h d) = "ChkFound {k=" ++ show k ++ ", h=" ++ (show h) ++ ", len=" ++ (show $ BS.length d) ++ "}"

chkPersist :: StorePersistable ChkRequest ChkFound
chkPersist = SP
  { storeSize = error "CHK store size"
  , storePut  = \(ChkFound k h d) -> put k >> put h >> putByteString d
  , storeGet  = \_ -> do
    (k, h, d) <- (,,) <$> get <*> get <*> getByteString 32768
    case mkChkFound k h d of
      Right df -> return df
      Left e   -> fail $ T.unpack e
  }
  
-- | find the routing key for a DataFound
instance DataFound ChkFound where
  dataFoundLocation (ChkFound k _ _) = k

mkChkFound :: Key -> ChkHeader -> BS.ByteString -> Either T.Text ChkFound
mkChkFound k h d
  | hash == (BSL.fromStrict $ unKey k) = Right $ ChkFound k h d
  | otherwise = Left "hash mismatch"
  where
    hash = bytestringDigest $ sha256 $ BSL.fromChunks [unChkHeader h, d]



-- |
-- given the secret crypto key (second part of URIs), an data found can be
-- decrypted to get the source data back
decryptChk
  :: Key                          -- ^ the secret crypto key (second part of URIs)
  -> ChkFound                     -- ^ the encrypted data together with their headers
  -> Either T.Text BS.ByteString  -- ^ the decrypted payload
decryptChk key (ChkFound _ header ciphertext)
  | len > BS.length plaintext = Left $ "invalid length"
  | mac /= BSL.fromStrict hash = Left $ "mac mismatch"
  | otherwise = Right plaintext
  where
    hash = chkHeaderHash header
    cipherLen = chkHeaderCipherLen header
    iv = BS.take 16 hash
    aes = initAES $ unKey key
    plaintext'' = decryptCTR aes iv $ BS.concat [ciphertext, cipherLen] -- TODO get rid of the concat
    (plaintext', lenbytes) = BS.splitAt (BS.length ciphertext) plaintext''
    len = fromIntegral $ runGet getWord16be $ BSL.fromStrict lenbytes
    plaintext = BS.take len plaintext'
    mac = bytestringDigest (hmacSha256 (BSL.fromStrict $ unKey key) (BSL.fromStrict plaintext''))
