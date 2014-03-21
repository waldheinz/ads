
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Data (
  
  -- * CHKs
  ChkHeader, mkChkHeader, unChkHeader,
  chkHeaderHash, chkHeaderCipherLen,

  -- * Successfully retrieved data
  DataFound, mkChkFound, dataFoundLocation,
  decryptDataFound, -- dataFoundPayload,
  
  DataHandler
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad.STM
import Crypto.Cipher.AES
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put ( putByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy ( fromStrict )
import Data.Digest.Pure.SHA
import qualified Data.Text as T

import Freenet.Base64
import Freenet.Types

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

chkHeaderCipherLen :: ChkHeader -> BS.ByteString
chkHeaderCipherLen = BS.drop 34 . unChkHeader

data DataFound
   = ChkFound !Key !ChkHeader !BS.ByteString -- headers and data
   deriving ( Eq )
            
instance Binary DataFound where
  put (ChkFound k h d) = put (1 :: Word8) >> put k >> put h >> putByteString d

  get = do
    t <- get :: Get Word8
    case t of
      1 -> do
        (k, h, d) <- (,,) <$> get <*> get <*> getByteString 32768
        case mkChkFound k h d of
          Right df -> return df
          Left e   -> fail $ T.unpack e
      _ -> fail $ "unknown type " ++ show t

--dataFoundPayload :: DataFound -> BSL.ByteString
--dataFoundPayload (ChkFound _ h d) = (encode h) `BSL.append` (BSL.fromStrict d)

-- | find the routing key for a DataFound
dataFoundLocation :: DataFound -> Key
dataFoundLocation (ChkFound k _ _) = k -- mkKey' $ BSL.toStrict $ bytestringDigest $ sha256 $ BSL.fromChunks [unChkHeader h, d]
     
instance Show DataFound where
  show (ChkFound k h d) = "ChkFound {k=" ++ show k ++ ", h=" ++ (show h) ++ ", len=" ++ (show $ BS.length d) ++ "}"

mkChkFound :: Key -> ChkHeader -> BS.ByteString -> Either T.Text DataFound
mkChkFound k h d
  | hash == (BSL.fromStrict $ unKey k) = Right $ ChkFound k h d
  | otherwise = Left "hash mismatch"
  where
    hash = bytestringDigest $ sha256 $ BSL.fromChunks [unChkHeader h, d]

-- |
-- given the secret crypto key (second part of URIs), an data found can be
-- decrypted to get the source data back
decryptDataFound
  :: Key                          -- ^ the secret crypto key (second part of URIs)
  -> DataFound                    -- ^ the encrypted data together with their headers
  -> Either T.Text BS.ByteString  -- ^ the decrypted payload
decryptDataFound key (ChkFound _ header ciphertext)
  | len > BS.length plaintext = Left $ "invalid length"
  | mac /= fromStrict hash = Left $ "mac mismatch"
  | otherwise = Right plaintext
  where
    hash = chkHeaderHash header
    cipherLen = chkHeaderCipherLen header
    iv = BS.take 16 hash
    aes = initAES $ unKey key
    plaintext'' = decryptCTR aes iv $ BS.concat [ciphertext, cipherLen] -- TODO get rid of the concat
    (plaintext', lenbytes) = BS.splitAt (BS.length ciphertext) plaintext''
    len = fromIntegral $ runGet getWord16be $ fromStrict lenbytes
    plaintext = BS.take len plaintext'
    mac = bytestringDigest (hmacSha256 (fromStrict $ unKey key) (fromStrict plaintext''))

type DataHandler = DataFound -> STM ()
