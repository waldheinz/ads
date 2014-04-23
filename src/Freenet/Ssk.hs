
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Freenet.Ssk (
  SskRequest(..), SskBlock(..), mkSskBlock,
  sskLocation, sskLocation', sskEncryptDocname,
  decompressSsk,

  -- * SSK Headers
  SskHeader, mkSskHeader, sskDataSize, sskHeaderSize,

  -- * DSA
  PubKey, mkPubKey, hashPubKey, pubKeySize
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad.ST ( runST )
import Data.Binary
import Data.Binary.Get
import Data.Bits ( (.&.), (.|.), shiftL )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Text.Encoding ( encodeUtf8 )

import qualified Crypto.PubKey.DSA as DSA
import Data.Binary.Put

import Freenet.Base64
import Freenet.Compression
import Freenet.Pcfb
import qualified Freenet.Rijndael as RD
import Freenet.Types
import Utils

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- |
-- the header of an SSK data block, consisting of:
--    0  word16     : hash algorithm, must be SHA256
--    2  word16     : symmetric cipher identifier
--    4  32 bytes   : E(H(docname))
--   36  36 bytes   : encrypted part of the headers
--   72  32 bytes   : signature parameter R
--  104  32 bytes   : signature parameter S
--  136             : bytes total length
newtype SskHeader = SskHeader { unSskHeader :: BS.ByteString } deriving ( Eq )

sskHeaderSize :: Int
sskHeaderSize = 136

instance Show SskHeader where
  show (SskHeader bs) = T.unpack $ toBase64' bs

instance Binary SskHeader where
  put (SskHeader h) = putByteString h
  get = SskHeader <$> getByteString sskHeaderSize

mkSskHeader :: BS.ByteString -> Either T.Text SskHeader
mkSskHeader bs
  | BS.length bs == sskHeaderSize = Right $ SskHeader bs
  | otherwise = Left $ "SSK header length must be 136 bytes, got " `T.append` T.pack (show $ BS.length bs)

sskHeaderHashId :: SskHeader -> Word16
sskHeaderHashId h = runGet getWord16be $ bsFromStrict $ unSskHeader h

-- |
-- Returns the signature (r, s) parameter for verifying the payload.
sskHeaderRS :: SskHeader -> (Integer, Integer)
sskHeaderRS h = (r, s) where
  r = bsToPosI $ BS.take 32 $ BS.drop 72 $ unSskHeader h
  s = bsToPosI $ BS.take 32 $ BS.drop 104 $ unSskHeader h

-- |
-- Returns the encrypted part of the header, which holds the
-- symmetric key needed to decrypt the data, and also the original
-- length of the plaintext.
sskHeaderEncrypted :: SskHeader -> BS.ByteString
sskHeaderEncrypted h = BS.take 36 $ BS.drop 36 $ unSskHeader h

sskHeaderEHDocname :: SskHeader -> BS.ByteString
sskHeaderEHDocname h = BS.take 32 $ BS.drop 4 $ unSskHeader h

--------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------
-- | size of the SSK payload
sskDataSize :: Int
sskDataSize = 1024

data SskBlock = SskBlock !Key !PubKey !SskHeader !BS.ByteString

instance Show SskBlock where
  show (SskBlock k _ h d) = "SskBlock {k=" ++ show k ++ ", h=" ++ (show h) ++ ", len=" ++ (show $ BS.length d) ++ "}"

instance StorePersistable SskBlock where
  storeSize = \_ -> 32 + pubKeySize + sskHeaderSize + sskDataSize
  storePut = \(SskBlock k pk h d) -> put k >> put pk >> put h >> putByteString d
  storeGet = do
    (k, pk, h, d) <- (,,,) <$> get <*> get <*> get <*> getByteString sskDataSize
    case mkSskBlock k h d pk of
      Right df -> return df
      Left e   -> fail $ T.unpack e

instance DataBlock SskBlock where
  dataBlockLocation (SskBlock loc _ _ _) = freenetLocation loc $ (2 `shiftL` 8) + 2
  decryptDataBlock                       = decryptSskBlock
--   dataBlockType                          = const $ (2 `shiftL` 8) + 2

instance Binary SskBlock where
  put = storePut
  get = storeGet

mkSskBlock
  :: Key                      -- ^ location
  -> SskHeader                -- ^ header
  -> BS.ByteString            -- ^ payload
  -> PubKey                   -- ^ public key needed for verifying the signature
  -> Either T.Text SskBlock
mkSskBlock k h d pk
  | sskHeaderHashId h /= 1 = Left "hash must be SHA-256"
  | DSA.verify dsaMod (unPublicKey pk) sig overallHash = result -- yes, we really have to try two times because Freenet has this
  | DSA.verify id (unPublicKey pk) sig overallHash = result     -- strange bug I don't really understand. :-/
  | otherwise = Left "signature did not verify"
  where
    result = Right  $ SskBlock k pk h d
    overallHash = bsToStrict $ bytestringDigest $ sha256 $ BSL.fromChunks [hashHeader, dataHash]
    dataHash =  bsToStrict $ bytestringDigest $ sha256 $ bsFromStrict d
    hashHeader = BS.take 72 $ unSskHeader h
    sig = uncurry DSA.Signature $ sskHeaderRS h

decryptSskBlock :: SskBlock -> Key -> Either T.Text (BS.ByteString, Int)
decryptSskBlock (SskBlock _ _ h d) key
--  | calg /= 2 = Left $ T.pack $ "unknown SSK crypto algorithm " ++ show calg
  | dataLength < (fromIntegral origDataLength) = Left $ "data length mismatch"
  | otherwise = Right (plaintext, fromIntegral origDataLength) -- BSL.take (fromIntegral origDataLength) plaintext
  where
    dataLength = BS.length plaintext
    plaintext = runST $ do
      pcfb <- mkPCFB docKey docIv
      pcfbDecipher pcfb d

    docKey = RD.initKey 32 $ BS.take 32 plainHeader
    docIv  = BS.take 32 plainHeader -- TODO: is this really a good idea? Freenet does so, we have no choice anyway, but still
    origDataLength = (runGet getWord16be $ bsFromStrict $ BS.take 2 $ BS.drop 32 plainHeader) .&. 0x7fff

    plainHeader = runST $ do
      pcfb <- mkPCFB headerKey headerIv
      pcfbDecipher pcfb $ sskHeaderEncrypted h

    headerKey = RD.initKey 32 $ unKey key
    headerIv = sskHeaderEHDocname h

-- |
-- for SSKs, the routing key is determined by
-- H(PK) and the encrypted document name's hash E(H(docname))
sskLocation
  :: Key    -- ^ the public key hash
  -> Key    -- ^ the crypto key (required to encrypt the docname)
  -> T.Text -- ^ the document name
  -> Key    -- ^ the resulting routing key
sskLocation hpk ckey docname = sskLocation' hpk ehd where
  ehd = sskEncryptDocname ckey docname

-- |
-- determines the location for a SSK document
sskLocation'
  :: Key    -- ^ hash (public key)
  -> Key    -- ^ encrypt ( hash ( docname ) )
  -> Key    -- ^ routing key
sskLocation' hpk ehd = mkKey' $ bsToStrict $ bytestringDigest $ sha256 $ BSL.fromChunks [unKey ehd, unKey hpk]

-- |
-- encrypts the hash of an SSK document name. this is needed
-- to determine the location of an SSK document
sskEncryptDocname
  :: Key    -- ^ the crypto key (second part of the SSK URI)
  -> T.Text -- ^ the document name (first path element of SSK URI)
  -> Key    -- ^ the encrypted document name
sskEncryptDocname ckey docname = mkKey' $ RD.encipher rjk dnh where
  rjk = RD.initKey 32 $ unKey ckey -- prepare encryption key
  dnh = bsToStrict $ bytestringDigest $ sha256 $ bsFromStrict (encodeUtf8 docname)

-----------------------------------------------------------------------------------------------
-- DSA
-----------------------------------------------------------------------------------------------

putMpi :: Integer -> Put
putMpi i = putWord16be (len * 8 - 8) >> putByteString bs where
  bs = i2bs i
  len = fromIntegral $ BS.length bs

getMpi :: Get Integer
getMpi = do
  len <- (\x -> (x + 8) `div` 8) <$> getWord16be
  bs <- getByteString $ fromIntegral len
  return $ bs2i bs

putGroup :: DSA.Params -> Put
putGroup (DSA.Params p g q) = putMpi p >> putMpi q >> putMpi g

getGroup :: Get DSA.Params
getGroup = do
  p <- getMpi
  q <- getMpi
  g <- getMpi
  return $ DSA.Params p g q

newtype PubKey = PK { unPublicKey :: DSA.PublicKey } deriving ( Show )

instance Binary PubKey where
  put = putPk
  get = do
    before <- bytesRead
    grp    <- getGroup
    y      <- getMpi
    after  <- bytesRead
    skip $ pubKeySize - (fromIntegral $ after - before)
    return $ PK (DSA.PublicKey grp y)

mkPubKey :: BS.ByteString -> Either T.Text PubKey
mkPubKey bs = case decodeOrFail (bsFromStrict bs) of
  Left (_, _, e) -> Left $ T.pack e
  Right (_, _, pk) -> Right pk

pubKeySize :: Int
pubKeySize = 1024

-- |
-- put without padding
putPublicKey :: PubKey -> Put
putPublicKey (PK (DSA.PublicKey grp y)) = putGroup grp >> putMpi y

hashPubKey :: PubKey -> Key
hashPubKey pk = mkKey' $ bsToStrict $ bytestringDigest $ sha256 $ runPut $ putPublicKey pk

putPk :: PubKey -> Put
putPk pk = putLazyByteString d >> putLazyByteString pad where
  pad = BSL.replicate (fromIntegral $ pubKeySize - (fromIntegral $ BSL.length d)) 0
  d   = runPut $ putPublicKey pk

dsaMod :: BS.ByteString -> BS.ByteString
dsaMod bs = padBs (BS.length bs) (i2bs i') where
   i' = i .&. mask
   i = bs2posI bs
   mask = 2 ^ (255 :: Integer) - 1

padBs :: Int -> BS.ByteString -> BS.ByteString
padBs pl b
   | l == pl = b
   | l > pl = error "already bigger than padded length"
   | otherwise = BS.replicate (pl - l) 0 <> b
   where
      l = BS.length b

bs2posI :: BS.ByteString -> Integer
bs2posI = BS.foldl' (\a b -> (256 * a) .|. (fromIntegral b)) 0

data SskRequest = SskRequest
                  { sskReqPkh      :: ! Key
                  , sskReqEhd      :: ! Key
                  , sskReqAlg      :: ! Word8
                  } deriving ( Show )

instance Binary SskRequest where
  put (SskRequest pk eh a) = put pk >> put eh >> put a
  get = SskRequest <$> get <*> get <*> get

instance DataRequest SskRequest where
  dataRequestLocation (SskRequest pkh ehd alg) = freenetLocation (sskLocation' pkh ehd) ((2 `shiftL` 8) + (fromIntegral alg))

decompressSsk :: CompressionCodec -> BSL.ByteString -> IO (Either T.Text BSL.ByteString)
decompressSsk codec inp = decompress codec $ BSL.drop 2 inp
