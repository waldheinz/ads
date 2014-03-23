
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Data (
  
  -- * CHKs
  ChkHeader, mkChkHeader, unChkHeader,
  chkHeaderHash, chkHeaderCipherLen,

  -- * SSKs
  SskHeader, mkSskHeader, sskDataSize, sskHeaderSize,
  
  -- * requesting data
  DataRequest(..), toDataRequest, dataRequestLocation,
  
  -- * Successfully retrieved data
  DataFound, mkChkFound, mkSskFound, dataFoundLocation,
  decryptDataFound
  ) where

import Control.Applicative ( (<$>), (<*>) )
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
import Freenet.Pcfb
import Freenet.Types
import Freenet.URI

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

--------------------------------------------------------------------------------
-- SSK data
--------------------------------------------------------------------------------

-- | the header required to verify a CHK data block
newtype SskHeader = SskHeader { unSskHeader :: BS.ByteString } deriving ( Eq )

sskHeaderSize :: Int
sskHeaderSize = 136

-- | size of the SSK payload
sskDataSize :: Int
sskDataSize = 1024

instance Show SskHeader where
  show (SskHeader bs) = T.unpack $ toBase64' bs

instance Binary SskHeader where
  put (SskHeader h) = putByteString h
  get = SskHeader <$> getByteString sskHeaderSize

mkSskHeader :: BS.ByteString -> Either T.Text SskHeader
mkSskHeader bs
  | BS.length bs == sskHeaderSize = Right $ SskHeader bs
  | otherwise = Left $ "SSK header length must be 136 bytes, got " `T.append` T.pack (show $ BS.length bs)



--------------------------------------------------------------------------------
-- found data
--------------------------------------------------------------------------------

data DataFound
   = ChkFound !Key !ChkHeader !BS.ByteString -- location, headers and data
   | SskFound !Key !SskHeader !BS.ByteString -- location, headers and data
   deriving ( Eq )

instance Show DataFound where
  show (ChkFound k h d) = "ChkFound {k=" ++ show k ++ ", h=" ++ (show h) ++ ", len=" ++ (show $ BS.length d) ++ "}"
  show (SskFound k h d) = "SskFound {k=" ++ show k ++ ", h=" ++ (show h) ++ ", len=" ++ (show $ BS.length d) ++ "}"
  
instance StorePersistable DataFound where
  storePersistFile (ChkFound _ _ _) = "store-chk"
  storePersistFile (SskFound _ _ _) = "store-ssk"
  
  storePersistPut  (ChkFound k h d) = put k >> put h >> putByteString d
  storePersistPut  (SskFound k h d) = put k >> put h >> putByteString d
  
  storePersistGet "store-chk" = do
    (k, h, d) <- (,,) <$> get <*> get <*> getByteString 32768
    case mkChkFound k h d of
      Right df -> return df
      Left e   -> fail $ T.unpack e
      
  storePersistGet "store-ssk" = do
    (k, h, d) <- (,,) <$> get <*> get <*> getByteString sskDataSize
    case mkSskFound k h d of
      Right df -> return df
      Left e   -> fail $ T.unpack e
  
  storePersistGet name = fail $ "unknown store: " ++ name
  
-- | find the routing key for a DataFound
dataFoundLocation :: DataFound -> Key
dataFoundLocation (ChkFound k _ _) = k
dataFoundLocation (SskFound k _ _) = k

mkChkFound :: Key -> ChkHeader -> BS.ByteString -> Either T.Text DataFound
mkChkFound k h d
  | hash == (BSL.fromStrict $ unKey k) = Right $ ChkFound k h d
  | otherwise = Left "hash mismatch"
  where
    hash = bytestringDigest $ sha256 $ BSL.fromChunks [unChkHeader h, d]

mkSskFound :: Key -> SskHeader -> BS.ByteString -> Either T.Text DataFound
mkSskFound k h d
  | otherwise = Right $ SskFound k h d

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

-------------------------------------------------------------------------------
-- requesting data
-------------------------------------------------------------------------------

data DataRequest
   = ChkRequest Key Word8     -- ^ the location and the hash algorithm so it can be verified
   | SskRequest Key Key Word8 -- ^ the pubkey hash, encrypted.hashed docname and algoritms
   deriving ( Show )

dataRequestLocation :: DataRequest -> Key
dataRequestLocation (ChkRequest k _) = k
dataRequestLocation (SskRequest hpk ehd _) = sskLocation hpk ehd

toDataRequest :: URI -> DataRequest
toDataRequest (CHK loc _ e _) = ChkRequest loc $ chkExtraCrypto e
toDataRequest (SSK hpk ck e doc _) = SskRequest hpk (sskEncryptDocname ck doc) (sskExtraCrypto e)
