
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Chk (
  -- * Working with CHKs
  ChkFound(..), mkChkFound,

  -- * CHK Headers
  ChkHeader, mkChkHeader, unChkHeader, chkHeaderHashId,
  chkHeaderHash, chkHeaderCipherLen
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad.ST ( runST )
import Crypto.Cipher.AES
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put ( putByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA
import qualified Data.Text as T

import Freenet.Base64
import Freenet.Pcfb
import qualified Freenet.Rijndael as RD
import Freenet.Store
import Freenet.Types

------------------------------------------------------------------------------
-- CHK headers
------------------------------------------------------------------------------

-- |
-- The header required to verify a CHK data block, consisting of
--    0   2    word16, hash id, always 1 for SHA256
--    2  32    SHA256(payload)
--   34   2    encrypted(word16) true payload size, before padding to 32kiB block size
--   ------
--   36        bytes total
newtype ChkHeader = ChkHeader { unChkHeader :: BS.ByteString } deriving ( Eq )

-- |
-- The size of a CHK block's header, which is 36 bytes.
chkHeaderSize :: Int
chkHeaderSize = 36

instance Show ChkHeader where
  show (ChkHeader bs) = T.unpack $ toBase64' bs

instance Binary ChkHeader where
  put (ChkHeader h) = putByteString h
  get = ChkHeader <$> getByteString chkHeaderSize

-- |
-- Creates right the header from a bytestring of 32 bytes.
mkChkHeader :: BS.ByteString -> Either T.Text ChkHeader
mkChkHeader bs
  | BS.length bs == chkHeaderSize = Right $ ChkHeader bs
  | otherwise = Left $ "CHK header length must be 36 bytes"

-- |
-- Returns the payload's SHA256 digest.
chkHeaderHash :: ChkHeader -> BS.ByteString
chkHeaderHash = BS.take 32 . BS.drop 2 . unChkHeader

-- |
-- The length of the CHK plaintext is stored encrypted
-- in it's header, and this function extracts it.
chkHeaderCipherLen :: ChkHeader -> BS.ByteString
chkHeaderCipherLen = BS.drop 34 . unChkHeader

-- |
-- The hash algoritm used to generate the digest at offset 2,
-- only a value of 1, indicating SHA256, is currently used.
chkHeaderHashId :: ChkHeader -> Word16
chkHeaderHashId = runGet get . BSL.fromStrict . unChkHeader

data ChkFound = ChkFound !Key !ChkHeader !BS.ByteString -- location, headers and data

instance Show ChkFound where
  show (ChkFound k h d) = "ChkFound {k=" ++ show k ++ ", h=" ++ (show h) ++ ", len=" ++ (show $ BS.length d) ++ "}"

-- |
-- Size of the CHK payload, which is 32kiB.
chkDataSize :: Int
chkDataSize = 32768

instance StorePersistable ChkFound where
  storeSize = \_ -> 32 + chkHeaderSize + chkDataSize
  storePut  = \(ChkFound k h d) -> put k >> put h >> putByteString d
  storeGet  = \_ -> do
    (k, h, d) <- (,,) <$> get <*> get <*> getByteString 32768
    case mkChkFound k h d of
      Right df -> return df
      Left e   -> fail $ T.unpack e
  
-- | find the routing key for a DataFound
instance DataFound ChkFound where
  dataFoundLocation (ChkFound k _ _) = k
  decryptDataFound = decryptChk

mkChkFound :: Key -> ChkHeader -> BS.ByteString -> Either T.Text ChkFound
mkChkFound k h d
  | hash == (BSL.fromStrict $ unKey k) = Right $ ChkFound k h d
  | otherwise = Left "hash mismatch"
  where
    hash = bytestringDigest $ sha256 $ BSL.fromChunks [unChkHeader h, d]

-- |
-- given the secret crypto key (second part of URIs), data found can be
-- decrypted to get the source data back
decryptChk
  :: ChkFound                     -- ^ the encrypted data together with their headers
  -> Key                          -- ^ the secret crypto key (second part of URIs)
  -> Word8                        -- ^ crypto algorithm used
  -> Either T.Text BSL.ByteString -- ^ the decrypted payload
decryptChk (ChkFound _ header ciphertext) key calg
  | calg == 3 = decryptChkAesCtr header ciphertext key
  | calg == 2 = decryptChkAesPcfb header ciphertext key
  | otherwise = Left $ T.pack $ "unknown CHK crypto algorithm " ++ show calg

decryptChkAesPcfb :: ChkHeader -> BS.ByteString -> Key -> Either T.Text BSL.ByteString
decryptChkAesPcfb header ciphertext key
  | predIv /= iv = Left "CHK hash mismatch"
  | len > BS.length plaintext' = Left $ T.pack $ "invalid CHK length " ++ show len
  | otherwise = Right (BSL.fromStrict plaintext)
  where
    (plaintext', headers') = runST $ do
      pcfb <- mkPCFB (RD.initKey 32 $ unKey key) $ BS.replicate 32 0 -- ^ the IV is all zero, but
      h' <- pcfbDecipher pcfb $ BS.drop 2 $ unChkHeader header       -- ^ this is said to serve as IV
      p  <- pcfbDecipher pcfb ciphertext
      return (p, h')
    plaintext = BS.take len plaintext'
    iv = BS.take 32 headers'
    predIv = BSL.toStrict $ bytestringDigest $ sha256 (BSL.fromStrict $ unKey key)
    len = fromIntegral $ runGet getWord16be $ BSL.fromStrict $ BS.drop 32 headers'
    
decryptChkAesCtr :: ChkHeader -> BS.ByteString -> Key -> Either T.Text BSL.ByteString
decryptChkAesCtr header ciphertext key
  | len > BS.length plaintext = Left "invalid length"
  | mac /= BSL.fromStrict hash = Left "mac mismatch when verifying CHK payload"
  | otherwise = Right (BSL.fromStrict plaintext)
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
