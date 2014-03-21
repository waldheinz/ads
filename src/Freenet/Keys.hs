
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Keys (
  CHK', decryptDataFound,

  -- * Metadata
  Doctype(..), parseMetadata
  ) where

import Control.Monad ( liftM2, replicateM, void, when )
import Crypto.Cipher.AES
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put ( putByteString )
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy ( fromStrict )
import Data.Digest.Pure.SHA
import Data.Maybe ( catMaybes )
import qualified Data.Text as T

import Freenet.Mime
import Freenet.Types
import Freenet.URI
import Types

newtype CHK' = CHK' BS.ByteString

chkPayloadSize :: Int
chkPayloadSize = 32 * 1024

instance Binary CHK' where
  put (CHK' p) = putByteString p
  get = fmap CHK' (getByteString chkPayloadSize)

instance Block CHK' where
  size _ = chkPayloadSize
  
decryptDataFound
  :: Key                          -- ^ the secret crypto key (second part of URIs)
  -> DataFound                    -- ^ the encrypted data together with their headers
  -> Either T.Text BS.ByteString  -- ^ the decrypted payload
decryptDataFound key (ChkFound _ header ciphertext) =
  let
    hash = chkHeaderHash header
    cipherLen = chkHeaderCipherLen header
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
     
---------------------------------------------------------------------------------------
-- Metadata Parsing
---------------------------------------------------------------------------------------

flagSet :: (Bits a) => a -> a -> Bool
flagSet flags flag = flags .&. flag == flag

data HashType = SHA1 | MD5 | SHA256 | SHA384 | SHA512 | ED2K | TTH
              deriving ( Bounded, Enum, Eq, Show )

hashTypeFlag :: HashType -> Word32
hashTypeFlag ht = 1 `shiftL` (fromEnum ht)

hashTypeSize :: HashType -> Int
hashTypeSize t = case t of
  SHA1   -> 20
  MD5    -> 16
  SHA256 -> 32
  SHA384 -> 48
  SHA512 -> 64
  ED2K   -> 16
  TTH    -> 24

getHashes :: Get [(HashType, BS.ByteString)]
getHashes = do
  mask <- getWord32be

  let
    go t
      | flagSet mask (hashTypeFlag t) = getByteString (hashTypeSize t) >>= (\h -> return (Just (t, h)))
      | otherwise = return Nothing

  result <- mapM go [minBound..]
  return $ catMaybes result

deriveCryptoKey :: [(HashType, BS.ByteString)] -> Maybe Key
deriveCryptoKey hashes = lookup SHA256 hashes >>= \h -> case mkey h of
  Left _ -> Nothing
  Right k -> Just k
  where
    mkey hash = mkKey $ BSL.toStrict $ bytestringDigest $ sha256 (BSL.fromChunks [hash, BSC.pack "SPLITKEY"] )

data Flag = FlagSplitFile | Hashes | SpecifySplitfileKey | HashThisLayer
          | TopSize | Compressed | NoMime | CompressedMime | Dbr
          | ExtraMetadata
          deriving ( Show )

flagBit :: Flag -> Word16
flagBit f = case f of
  FlagSplitFile       -> 1
  Dbr                 -> 2
  NoMime              -> 4
  CompressedMime      -> 8
  ExtraMetadata       -> 16
  Compressed          -> 128
  TopSize             -> 256
  Hashes              -> 512
  SpecifySplitfileKey -> 1024
  HashThisLayer       -> 2048

data CompressionCodec = None | Bzip2 deriving ( Show )

getMime
  :: Word16    -- ^ doctype - level flags
  -> Get (Maybe Mime)
getMime flags =
  if (flagSet flags (flagBit NoMime))
  then return Nothing
  else if (flagSet flags (flagBit CompressedMime))
       then do
         x <- getWord16be
         when (x > 32767) $ void getWord16be -- compessed MIME params, whatever?
         case lookup x defaultMimes of
           Nothing   -> fail $ "unknown default mime " ++ show x
           Just mime -> return $ Just mime
       else fail "only compressed MIME supported"

data SplitFileSegment
  = SplitFileSegment
    { sfUri :: ! URI
    }
    deriving ( Show )

getSplitFileSegment :: Maybe (Key, Word8) -> Get SplitFileSegment
getSplitFileSegment common =
  case common of
    Just (key, crypt) -> do
      location <- getByteString 32
      let
        lk = mkKey' location
        ex = mkChkExtra crypt (-1) False
        
      return $! SplitFileSegment $ CHK lk key ex
      
    Nothing -> fail "only shared crypto infos allowed for now"

data RedirectTarget
  = SplitFile
    { sfCompression :: CompressionCodec
    , sfSegments    :: [SplitFileSegment]
    }
    deriving ( Show )
             
getSplitFile
  :: Word16     -- ^ flags
  -> Maybe Key  -- ^ (single crypto key for all segments), if those are common
  -> Get RedirectTarget
getSplitFile flags mkey = do
  cryptoAlgo <- getWord8
  dlen <- getWord64be
  
  (ccodec, olen) <- if (flagSet flags (flagBit Compressed))
                    then do
                      c <- getWord16be >>= \c -> case c of
                        1 -> return Bzip2
                        x -> fail $ "unknown compressor " ++ show x
                      len <- getWord64be
                      return (c, len)
                    else return (None, dlen)

  mime <- getMime flags
  sfAlgo <- getWord16be
  
  case sfAlgo of
    1 -> do -- SPLITFILE_ONION_STANDARD
      paramsLen <- getWord32be
      when (paramsLen > 32768) $ fail ("too much splitfile parameter data: " ++ show paramsLen)
      when (paramsLen /= 10) $ fail ("need 10 splitfile param bytes for version 1, got " ++ show paramsLen)
      (blocksPerSegment, checkBlocks) <- getWord16be >>= \t -> case t of
        0 -> do  -- SPLITFILE_PARAMS_SIMPLE_SEGMENT
          liftM2 (,) getWord32be getWord32be
        x  -> fail $ "unknown splitfile params type " ++ show x

      splitfileBlocks <- getWord32be
      splitfileCheckBlocks <- getWord32be
      
      let
        segmentCount = (splitfileBlocks + blocksPerSegment - 1) `div` blocksPerSegment
        gsfsParams = mkey >>= \k -> Just (k, cryptoAlgo)

      when (segmentCount /= 1) $ fail "only single-segment splitfiles for now"

      dataBlocks  <- replicateM (fromIntegral splitfileBlocks)      $ getSplitFileSegment gsfsParams
      checkBlocks <- replicateM (fromIntegral splitfileCheckBlocks) $ getSplitFileSegment gsfsParams
      
      return $! SplitFile ccodec $ dataBlocks ++ checkBlocks
      
    x -> fail $ "unknown splitfile algo " ++ show x

data Doctype
  = SimpleRedirect
    { srHashes      :: [(HashType, BS.ByteString)]
    , srTarget      :: RedirectTarget
    }
    deriving ( Show )

getSimpleRedirect :: Get Doctype
getSimpleRedirect = do
  flags <- getWord16be

  hashes <- if flagSet flags (flagBit Hashes)
    then getHashes 
    else return []

  when (not (elem SHA256 $ map fst hashes)) $ fail "no SHA256 hash specified"
  when (flagSet flags (flagBit HashThisLayer)) $ fail "unsupported hashThisLayer flag set"
  when (flagSet flags (flagBit TopSize)) $ fail "unsupported topSize flag set"
  when (flagSet flags (flagBit SpecifySplitfileKey)) $ fail "unsupported specifySplitfileKey flag set"
  when (flagSet flags (flagBit Dbr)) $ fail "unsupported dbr flag set"
  when (flagSet flags (flagBit ExtraMetadata)) $ fail "unsupported extraMetadata flag set"
  
  target <- if flagSet flags (flagBit FlagSplitFile)
            then getSplitFile flags (deriveCryptoKey hashes)
            else fail $ "unknown redirect target type"
                 
  return $! SimpleRedirect hashes target 
             
instance Binary Doctype where
  put _ = error "can't write Doctypes yet"
  
  get = do
    magic <- getWord64be
    version <- getWord16be
    doctype <- getWord8

    if magic /= 0xf053b2842d91482b
      then fail "missing magic"
      else case (version, doctype) of
        (1, 0)  -> getSimpleRedirect
        dv      -> fail $ "unknown doctype/version " ++ show dv
    
parseMetadata :: BS.ByteString -> Either T.Text Doctype
parseMetadata bs = case decodeOrFail (fromStrict bs) of
  Left (_, off, e) -> Left $ T.concat [T.pack e, " at ", T.pack $ show off]
  Right(_, _, md)  -> Right md
                                                         
