
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Metadata (
  -- * Metadata
  Metadata(..), parseMetadata, CompressionCodec(..),

  -- * Redirects
  RedirectTarget(..),

  -- * Splitfiles
  SplitFileSegment(..)
  ) where

import Control.Monad ( liftM2, replicateM, void, when )
import Data.Binary
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy ( fromStrict )
import Data.Digest.Pure.SHA
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8' )

import Freenet.Mime
import Freenet.Types
import Freenet.URI
     
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
          | TopSize | FullKeys | Compressed | NoMime | CompressedMime | Dbr
          | ExtraMetadata
          deriving ( Show )

flagBit :: Flag -> Word16
flagBit f = case f of
  FlagSplitFile       -> 1
  Dbr                 -> 2
  NoMime              -> 4
  CompressedMime      -> 8
  ExtraMetadata       -> 16
  FullKeys            -> 32
  Compressed          -> 128
  TopSize             -> 256
  Hashes              -> 512
  SpecifySplitfileKey -> 1024
  HashThisLayer       -> 2048

data CompressionCodec = None | Gzip | Bzip2 deriving ( Show )

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
    { sfUri   :: ! URI   -- ^ the URI where this segment can be fetched
    , sfsData :: ! Bool  -- ^ True if this is a data block, false if it's a check block
    }
    deriving ( Show )

getSplitFileSegment
  :: Maybe (Key, Word8)    -- ^ maybe common (decryption key, algorithm)
  -> Bool                  -- ^ if this segment references a data block (it's a FEC check block otherwise)
  -> Get SplitFileSegment
getSplitFileSegment common isData =
  case common of
    Just (key, crypt) -> do
      location <- getByteString 32
      let
        lk = mkKey' location
        ex = mkChkExtra crypt (-1) False
        
      return $! SplitFileSegment (CHK lk key ex []) isData
      
    Nothing -> fail "only shared crypto infos allowed for now"

data RedirectTarget
  = SplitFile
    { sfCompression    :: CompressionCodec   -- ^ the compression codec used by this splitfile
    , sfCompressedSize :: Word64             -- ^ size of compressed data, equals original size if not compressed
    , sfOriginalSize   :: Word64             -- ^ size of original data before compression was applied
    , sfSegments       :: [SplitFileSegment] -- ^ the segments this split consists of
    , sfMime           :: Maybe Mime         -- ^ MIME type of the target data
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
                        0 -> return Gzip
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

      dataBlocks  <- replicateM (fromIntegral splitfileBlocks)      $ getSplitFileSegment gsfsParams True
      checkBlocks <- replicateM (fromIntegral splitfileCheckBlocks) $ getSplitFileSegment gsfsParams False
      
      return $! SplitFile ccodec dlen olen (dataBlocks ++ checkBlocks) mime
      
    x -> fail $ "unknown splitfile algo " ++ show x

--------------------------------------------------------------------------
-- top-level metadata
--------------------------------------------------------------------------

data Metadata
  = SimpleRedirect
    { srHashes      :: [(HashType, BS.ByteString)]
    , srTarget      :: RedirectTarget
    }
  | Manifest
    { mEntries      :: [(T.Text, Metadata)]
    }
  | ArchiveManifest
    { amUri    :: URI
    , amMime   :: Maybe Mime
    , amType   :: ArchiveManifestType
    , amHashes :: [(HashType, BS.ByteString)]
    }
  deriving ( Show )

data ArchiveManifestType = ZIP | TAR deriving ( Eq, Show )

getArchiveManifest :: Get Metadata
getArchiveManifest = do
  flags <- getWord16be
  
  hashes <- if flagSet flags (flagBit Hashes)
    then getHashes 
    else return []

  atype <- do
    tw <- getWord16be
    case tw of
      0 -> return ZIP
      1 -> return TAR
      x -> fail $ "unknown archive type " ++ show x
      
  mime <- getMime flags

  when (flagSet flags (flagBit HashThisLayer)) $ fail "unsupported hashThisLayer flag set"
  when (flagSet flags (flagBit TopSize)) $ fail "unsupported topSize flag set"
  when (flagSet flags (flagBit SpecifySplitfileKey)) $ fail "unsupported specifySplitfileKey flag set"
  when (flagSet flags (flagBit Dbr)) $ fail "unsupported dbr flag set"
  when (flagSet flags (flagBit ExtraMetadata)) $ fail "unsupported extraMetadata flag set"

  key <- if flagSet flags (flagBit FullKeys)
         then fail "cannot parse full-key archive manifests"
         else do
           e <- get -- yeah, store extra first so we can't parse with Applicative
           (l, c) <- liftM2 (,) get get
           return $ CHK l c e []

  return $ ArchiveManifest key mime atype hashes
  
getManifestEntry :: Get (T.Text, Metadata)
getManifestEntry = do
  nameLen <- getWord16be
  nameBytes <- getByteString (fromIntegral nameLen)

  case decodeUtf8' nameBytes of
    Left e     -> fail $ "invalid UTF8 in entry name " ++ show e
    Right name -> do
      mLen <- getWord16be
      mBytes <- getLazyByteString (fromIntegral mLen)
      case decodeOrFail mBytes of
        Left (_, _, e) -> fail $ "error parsing manifest entry \"" ++ T.unpack name ++ "\": " ++ e
        Right  (_, _, m) -> return (name, m)

getSimpleManifest :: Get Metadata
getSimpleManifest = do
  entryCount <- getWord32be
  entries <- replicateM (fromIntegral entryCount) getManifestEntry
  return $ Manifest entries

getSimpleRedirect :: Get Metadata
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

instance Binary Metadata where
  put _ = error "can't write Doctypes yet"
  
  get = do
    magic <- getWord64be
    version <- getWord16be
    doctype <- getWord8

    if magic /= 0xf053b2842d91482b
      then fail "missing magic"
      else case (version, doctype) of
        (0, 2) -> getSimpleManifest
        (1, 0) -> getSimpleRedirect
        (1, 3) -> getArchiveManifest
        vd     -> fail $ "unknown version/doctype " ++ show vd
    
parseMetadata :: BS.ByteString -> Either T.Text Metadata
parseMetadata bs = case decodeOrFail (fromStrict bs) of
  Left (_, off, e) -> Left $ T.concat [T.pack e, " at ", T.pack $ show off]
  Right(_, _, md)  -> Right md
                                                         
