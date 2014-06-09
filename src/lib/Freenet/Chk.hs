
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Chk (
  -- * Working with CHKs
  ChkRequest(..), ChkBlock(..), mkChkBlock, decompressChk,
  encryptChk, ChkInsert(..), chkBlockUri, chkDataSize,
  decryptChk,

  -- * CHK Headers
  ChkHeader, mkChkHeader, unChkHeader, chkHeaderHashId,
  chkHeaderHash, chkHeaderCipherLen
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( mzero )
import Control.Monad.ST ( runST )
import Crypto.Cipher.AES
import Data.Aeson
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put ( putByteString, putWord16be, runPut )
import Data.Bits ( (.&.), shiftL )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA
import qualified Data.Text as T

import Freenet.Base64
import Freenet.Compression
import Freenet.Pcfb
import qualified Freenet.Rijndael as RD
import Freenet.Types
import Freenet.URI
import Utils

class ChkInsert a where
  insertChk :: a -> ChkBlock -> IO ()

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
-- Creates right the header from a bytestring of 36 bytes.
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
chkHeaderHashId = runGet get . bsFromStrict . unChkHeader

data ChkBlock = ChkBlock
                { chkBlockKey    :: ! Key           -- ^ location
                , chkBlockCrypto :: ! Word8         -- ^ crypto algorithm, 2 -> AES_PCFB_256_SHA256, 3 -> AES_CTR_256_SHA256
                , chkBlockHeader :: ! ChkHeader     -- ^ headers
                , chkBlockData   :: ! BS.ByteString -- ^ data
                }

instance Show ChkBlock where
  show (ChkBlock k _ h d) = "ChkFound {k=" ++ show k ++ ", h=" ++ (show h) ++ ", len=" ++ (show $ BS.length d) ++ "}"

instance ToJSON ChkBlock where
  toJSON (ChkBlock k c h d) = object
                              [ "location"  .= k
                              , "algorithm" .= c
                              , "header"    .= (toJSON . toBase64' . unChkHeader) h
                              , "data"      .= (toJSON . toBase64') d
                              ]

-- |
-- Size of the CHK payload, which is 32kiB.
chkDataSize :: Num a => a
chkDataSize = 32768

instance StorePersistable ChkBlock where
  storeSize = const $ 32 + chkHeaderSize + chkDataSize + 1 -- first is key and last is crypto
  storePut  = \(ChkBlock k calg h d) -> put k >> put calg >> put h >> putByteString d
  storeGet  = do
    (k, calg, h, d) <- (,,,) <$> get <*> get <*> get <*> getByteString chkDataSize
    case mkChkBlock k h d calg of
      Right df -> return df
      Left e   -> fail $ T.unpack e
  
-- | find the routing key for a DataFound
instance DataBlock ChkBlock where
  dataBlockLocation (ChkBlock k c _ _)   = freenetLocation k $ (1 `shiftL` 8) + (fromIntegral $ c .&. 0xff)
  decryptDataBlock                       = decryptChk
  
instance Binary ChkBlock where
  put = storePut
  get = storeGet

chkBlockUri
  :: ChkBlock
  -> Key
  -> URI
chkBlockUri blk ck = CHK (chkBlockKey blk) ck (mkChkExtra 3 (-1) False) []

-- |
-- creates a @ChkBlock@ from it's ingredients, verifying the hash and size of
-- the data block
mkChkBlock :: Key -> ChkHeader -> BS.ByteString -> Word8 -> Either T.Text ChkBlock
mkChkBlock k h d calg
  | hash /= (bsFromStrict $ unKey k) = Left "hash mismatch"
  | BS.length d /= chkDataSize = Left "CHK data must be 32kiB"
  | otherwise = Right $ ChkBlock k calg h d
  where
    hash = bytestringDigest $ sha256 $ BSL.fromChunks [unChkHeader h, d]

-- |
-- given the secret crypto key (second part of URIs), data found can be
-- decrypted to get the source data back
decryptChk
  :: ChkBlock                           -- ^ the encrypted data together with their headers
  -> Key                                -- ^ the secret crypto key (second part of URIs)
  -> Either T.Text (BS.ByteString, Int) -- ^ (decrypted payload, original length)
decryptChk (ChkBlock _ calg header ciphertext) key
  | calg == 3 = decryptChkAesCtr header ciphertext key
  | calg == 2 = decryptChkAesPcfb header ciphertext key
  | otherwise = Left $ T.pack $ "unknown CHK crypto algorithm " ++ show calg

decryptChkAesPcfb :: ChkHeader -> BS.ByteString -> Key -> Either T.Text (BS.ByteString, Int)
decryptChkAesPcfb header ciphertext key
  | predIv /= iv = Left "CHK hash mismatch"
  | len > BS.length plaintext' = Left $ T.pack $ "invalid CHK length " ++ show len
  | otherwise = Right (plaintext, len)
  where
    (plaintext', headers') = runST $ do
      pcfb <- mkPCFB (RD.initKey 32 $ unKey key) $ BS.replicate 32 0 -- ^ the IV is all zero, but
      h' <- pcfbDecipher pcfb $ BS.drop 2 $ unChkHeader header       -- ^ this is said to serve as IV
      p  <- pcfbDecipher pcfb ciphertext
      return (p, h')
    plaintext = plaintext' -- BS.take len plaintext'
    iv = BS.take 32 headers'
    predIv = bsToStrict $ bytestringDigest $ sha256 (bsFromStrict $ unKey key)
    len = fromIntegral $ runGet getWord16be $ bsFromStrict $ BS.drop 32 headers'
    
decryptChkAesCtr :: ChkHeader -> BS.ByteString -> Key -> Either T.Text (BS.ByteString, Int)
decryptChkAesCtr header ciphertext key
  | len > BS.length plaintext' = Left "invalid length"
  | mac /= bsFromStrict hash = Left "mac mismatch when verifying CHK payload"
  | otherwise = Right (plaintext', len)
  where
    hash = chkHeaderHash header
    cipherLen = chkHeaderCipherLen header
    iv = BS.take 16 hash
    aes = initAES $ unKey key
    plaintext'' = decryptCTR aes iv $ BS.concat [ciphertext, cipherLen] -- TODO get rid of the concat
    (plaintext', lenbytes) = BS.splitAt (BS.length ciphertext) plaintext''
    len = fromIntegral $ runGet getWord16be $ bsFromStrict lenbytes
    mac = bytestringDigest (hmacSha256 (bsFromStrict $ unKey key) (bsFromStrict plaintext''))

-- |
-- encrypts some data (which must be <= chkDataSize in length) using some encryption
-- key, and returns the resulting @ChkBlock@
encryptChk :: BS.ByteString -> Key -> ChkBlock
encryptChk d k
  | payloadSize > chkDataSize = error "CHK payload > 32kiB"
  | otherwise = ChkBlock loc 3 hdr ciphertext
  where
    hdr         = ChkHeader $ bsToStrict $ runPut $ putWord16be 1 >> putByteString mac >> putByteString cipherLen
    payloadSize = BS.length d
    padding     = BS.replicate (chkDataSize - payloadSize) 0
    plaintext   = BS.concat [d, padding, bsToStrict $ runPut $ putWord16be $ fromIntegral payloadSize]
    (ciphertext, cipherLen) = BS.splitAt chkDataSize $ encryptCTR aes iv plaintext
    aes = initAES $ unKey k
    iv = BS.take 16 mac
    mac = bsToStrict $ bytestringDigest $ hmacSha256 (bsFromStrict $ unKey k) (bsFromStrict plaintext)
    loc = mkKey' $ bsToStrict $ bytestringDigest $ sha256 $ BSL.fromChunks [unChkHeader hdr, ciphertext]

-------------------------------------------------------------------------------------------------
-- Requesting CHKs
-------------------------------------------------------------------------------------------------

data ChkRequest = ChkRequest
                  { chkReqLocation :: ! Key   -- ^ the location of the data
                  , chkReqHashAlg  :: ! Word8 -- ^ the hash algorithm to use
                  } deriving ( Show )
                  
instance Binary ChkRequest where
  put (ChkRequest l h) = put l >> put h
  get = ChkRequest <$> get <*> get

instance FromJSON ChkRequest where
  parseJSON (Object v) = ChkRequest
                         <$> v .: "location"
                         <*> v .: "algorithm"
  parseJSON _ = mzero

instance DataRequest ChkRequest where
  dataRequestLocation (ChkRequest l a) = freenetLocation l $ (1 `shiftL` 8) + (fromIntegral $ a .&. 0xff)

decompressChk
  :: CompressionCodec -- ^ codec to use
  -> BS.ByteString    -- ^ compressed data
  -> Int              -- ^ true compressed data length
  -> IO (Either T.Text (BS.ByteString, Int))
decompressChk codec inp inpl
  | codec == None = return $ Right (inp, inpl)
  | otherwise     = do
    dec <- decompress codec $ BSL.drop 4 $ bsFromStrict (BS.take inpl inp)
    case dec of
      Left e     -> return $ Left $ "error decompressing data: " `T.append` e
      Right dec' -> return $ Right (bsToStrict dec', fromIntegral $ BSL.length dec')
