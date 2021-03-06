
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Metadata (
  -- * Metadata
  Metadata(..), parseMetadata,  ManifestEntries,
  HashType(..),

  -- * Redirects
  RedirectTarget(..),

  -- * Archive Manifests
  TopBlock(..), ArchiveType(..)
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( liftM2, replicateM, unless, void, when )
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA
import Data.List ( foldl' )
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8', encodeUtf8 )

import Freenet.Compression
import Freenet.Mime
import Freenet.SplitFile
import Freenet.Types
import Freenet.URI
import Utils
  
---------------------------------------------------------------------------------------
-- Metadata Parsing
---------------------------------------------------------------------------------------

data ArchiveType = ZIP | TAR deriving ( Eq, Ord, Show )

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
    mkey hash = mkKey $ bsToStrict $ bytestringDigest $ sha256 (BSL.fromChunks [hash, BSC.pack "SPLITKEY"] )

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
         return $ Just $ lookupMime defaultMimes x
       else do
         len   <- getWord8
         bytes <- getByteString (fromIntegral len)
         case decodeUtf8' bytes of
           Left e    -> fail $ "invalid UTF8 in uncompressed MIME string " ++ show e
           Right tgt -> return $ Just tgt

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
      
    Nothing -> do
      e <- get
      (l, c) <- liftM2 (,) get get
      return $! SplitFileSegment (CHK l c e []) isData

data RedirectTarget
  = RedirectSplitFile SplitFile
  | RedirectKey
    { rkMime :: Maybe Mime
    , rkUri  :: URI
    }
    deriving ( Eq, Ord, Show )

getCompression
  :: Word16                         -- ^ flags
  -> Word64                         -- ^ data length 
  -> Get (CompressionCodec, Word64) -- ^ (compression codec, decomressed size)
getCompression flags dlen =
  if (flagSet flags (flagBit Compressed))
  then do
    c <- getWord16be >>= \c -> case c of
      0 -> return Gzip
      1 -> return Bzip2
      2 -> return LZMA
      3 -> return LZMA_NEW
      x -> fail $ "unknown compressor " ++ show x
    len <- getWord64be
    return (c, len)
  else return (None, dlen)

getSplitFile
  :: Version
  -> Word16        -- ^ flags
  -> Maybe Key     -- ^ (single crypto key for all segments), if those are common
  -> Word8         -- ^ single crypto algorihm for all segments
  -> Get SplitFile
getSplitFile v flags mkey cryptoAlgo = do
  dlen <- getWord64be
  
  (ccodec, olen) <- getCompression flags dlen
    
  mime <- getMime flags
  sfAlgo <- getWord16be
  
  case sfAlgo of
    1 -> do -- SPLITFILE_ONION_STANDARD
      paramsLen <- getWord32be
      when (paramsLen > 32768) $ fail ("too much splitfile parameter data: " ++ show paramsLen)
      when (v == V1 && paramsLen /= 10) $ fail ("need 10 splitfile param bytes for version 1, got " ++ show paramsLen)
      when (v == V0 && paramsLen < 8) $ fail ("need >=8 splitfile param bytes for version 0, got " ++ show paramsLen)
      
      (blocksPerSegment, checkBlocks, crossCheckBlocks) <- case v of
        V0 -> do
          p1 <- bytesRead
          (bps, cbs) <- liftM2 (,) getWord32be getWord32be
          p2 <- bytesRead
          skip $ (fromIntegral paramsLen) - (fromIntegral $ p2 - p1)
          return (bps, cbs, 0)
        V1 -> getWord16be >>= \t -> case t of
          0 -> do  -- SPLITFILE_PARAMS_SIMPLE_SEGMENT
            (bps, cbs) <- liftM2 (,) getWord32be getWord32be
            return (bps, cbs, 0)
          x  -> fail $ "unknown splitfile params type " ++ show x
        
      splitfileBlocks <- getWord32be
      splitfileCheckBlocks <- getWord32be
      
      let
        segmentCount = (splitfileBlocks + blocksPerSegment + crossCheckBlocks - 1) `div` (blocksPerSegment + crossCheckBlocks)
        gsfsParams = mkey >>= \k -> Just (k, cryptoAlgo)

      when (segmentCount /= 1) $ fail "only single-segment splitfiles for now"

      dataBlocks  <- replicateM (fromIntegral splitfileBlocks)      $ getSplitFileSegment gsfsParams True
      checkBlocks' <- replicateM (fromIntegral splitfileCheckBlocks) $ getSplitFileSegment gsfsParams False
      
      return $! SplitFile ccodec dlen olen (dataBlocks ++ checkBlocks') mime
      
    x -> fail $ "unknown splitfile algo " ++ show x

--------------------------------------------------------------------------
-- top-level metadata
--------------------------------------------------------------------------

type ManifestEntries = [(T.Text, Metadata)]

data Metadata
  = SimpleRedirect
    { srHashes      :: [(HashType, BS.ByteString)]
    , srTarget      :: RedirectTarget
    }
  | Manifest
    { mEntries      :: ManifestEntries
    }
  | MultiLevel
    { mlSplitfile   :: SplitFile
    }
  | ArchiveManifest
    { amTarget      :: RedirectTarget
    , amType        :: ArchiveType
    , amHashes      :: [(HashType, BS.ByteString)]
    , amCompCodec   :: CompressionCodec
    }
  | SymbolicShortlink
    { slTarget      :: T.Text
    }
  | ArchiveMetadataRedirect
    { amrTarget     :: T.Text
    }
  | ArchiveInternalRedirect
    { airTarget     :: T.Text
    , airMime       :: Maybe Mime
    }
  deriving ( Show )

getArchiveInternalRedirect :: Get Metadata
getArchiveInternalRedirect = do
  flags    <- getWord16be
  -- only NoMime / CompressedMime / (nothing) has ever been seen
  unless (flags == 8 || flags == 4 || flags == 0) $ fail $ "unexpected flags on archive internal redirect " ++ show flags
  mime <- getMime flags
  tgt <- getText
  return $ ArchiveInternalRedirect tgt mime

getArchiveMetadataRedirect :: Get Metadata
getArchiveMetadataRedirect = do
  flags    <- getWord16be
  unless (flags == 4) $ fail $ "unexpected flags on symbolic short link " ++ show flags
  tgt <- getText
  return $ ArchiveMetadataRedirect tgt

getText :: Get T.Text
getText = do
  tLen   <- getWord16be
  tBytes <- getByteString (fromIntegral tLen)
  case decodeUtf8' tBytes of
    Left e    -> fail $ "invalid UTF8 in metadata string " ++ show e
    Right tgt -> return tgt
  
getSymbolicShortlink :: Get Metadata
getSymbolicShortlink = do
  flags    <- getWord16be
  -- only NoMIME has ever been seen
  unless (flags == 4) $ fail $ "unexpected flags on symbolic short link " ++ show flags
  tgt <- getText
  return $ SymbolicShortlink tgt

data TopBlock = TopBlock
                { topSize           :: Word64
                , topCompressedSize :: Word64
                , topBlocksRequired :: Word32
                , topBlocksTotal    :: Word32
                , topDontCompress   :: Word8
                , topCompatMode     :: Word16
                }
                deriving ( Show )

getTopBlock :: Get TopBlock
getTopBlock = TopBlock <$> get <*> get <*> get <*> get <*> get <*> get

getArchiveManifest :: Version -> Get Metadata
getArchiveManifest v = do
  flags <- getWord16be
  (hashes, target, (Just atype)) <- getSimpleRedirect' v flags True
  return $ ArchiveManifest target atype hashes None

getMultiLevel :: Version -> Get Metadata
getMultiLevel v = do
  flags <- getWord16be

  hashes <- if flagSet flags (flagBit Hashes)
            then getHashes 
            else return []
  
  when (v == V1 && not (elem SHA256 $ map fst hashes)) $ fail "no SHA256 hash specified in V1 redirect on multilevel"
  when (flagSet flags (flagBit HashThisLayer)) $ fail "unsupported hashThisLayer flag set on multilevel"
  when (flagSet flags (flagBit Dbr)) $ fail "unsupported dbr flag set on multilevel"
  when (flagSet flags (flagBit ExtraMetadata)) $ fail "unsupported extraMetadata flag set on multilevel"
  unless (flagSet flags (flagBit FlagSplitFile)) $ fail "expected multilevel being a splitfile"
  
  calg <- if v == V1
          then getWord8 -- single crypto algorithm
          else return 2 -- ALGO_AES_PCFB_256_SHA256
  key <- if flagSet flags (flagBit SpecifySplitfileKey)
         then Just . mkKey' <$> getByteString 32
         else return $ deriveCryptoKey hashes
              
  MultiLevel <$> getSplitFile v flags key calg

getArchiveManifestType :: Get ArchiveType
getArchiveManifestType = do
  tw <- getWord16be
  case tw of
    0 -> return ZIP
    1 -> return TAR
    x -> fail $ "unknown archive type " ++ show x

getKey
  :: Word16 -- ^ flags
  -> Get URI
getKey flags =
  if flagSet flags (flagBit FullKeys)
  then do
    -- full keys
    kl <- getWord16be
    kb <- getLazyByteString $ fromIntegral kl
    
    case decodeOrFail kb of
      Left  (_, _, e) -> fail $ "error reading full key:" ++ e
      Right (_, _, k) -> return k
      
  else do
    -- short keys -- store extra first so we can't parse with Applicative, great
    e <- get
    (l, c) <- liftM2 (,) get get
    return $ CHK l c e []
  
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
        Left  (_, _, e) -> fail $ "error parsing manifest entry \"" ++ T.unpack name ++ "\": " ++ e
        Right (_, _, m) -> return (name, m)

getSimpleManifest :: Get Metadata
getSimpleManifest = do
  entryCount <- getWord32be
  entries <- replicateM (fromIntegral entryCount) getManifestEntry
  return $ Manifest entries

getSimpleRedirect :: Version -> Get Metadata
getSimpleRedirect v = do
  flags <- getWord16be
  (hashes, target, _) <- getSimpleRedirect' v flags False
  return $! SimpleRedirect hashes target 

getSimpleRedirect' :: Version -> Word16 -> Bool -> Get ([(HashType, BS.ByteString)], RedirectTarget, Maybe ArchiveType)
getSimpleRedirect' v flags isArchive = do
  hashes <- if flagSet flags (flagBit Hashes)
    then getHashes 
    else return []

  when (flagSet flags (flagBit TopSize)) $ void getTopBlock

  atype <- if isArchive
           then Just <$> getArchiveManifestType
           else return Nothing
  
  when (v == V1 && not (elem SHA256 $ map fst hashes)) $ fail "no SHA256 hash specified in V1 redirect"
  when (flagSet flags (flagBit HashThisLayer)) $ fail "unsupported hashThisLayer flag set"
  when (flagSet flags (flagBit Dbr)) $ fail "unsupported dbr flag set"
  when (flagSet flags (flagBit ExtraMetadata)) $ fail "unsupported extraMetadata flag set"
  
  target <- if flagSet flags (flagBit FlagSplitFile)
            then do
        --      when (v == V0) $ fail "can't read V0 splitfile"
              calg <- if v == V1
                      then getWord8 -- single crypto algorithm
                      else return 2 -- ALGO_AES_PCFB_256_SHA256
              key <- if flagSet flags (flagBit SpecifySplitfileKey)
                     then Just . mkKey' <$> getByteString 32
                     else return $ deriveCryptoKey hashes
              RedirectSplitFile <$> getSplitFile v flags key calg
            else do
              -- unless (flags == 40) $ fail $ "unsupported flags for key redirect " ++ show flags
              mime <- getMime flags
              RedirectKey mime <$> getKey flags
                 
  return $! (hashes, target, atype)

data Version = V0 | V1 deriving ( Eq )

magic :: Word64
magic = 0xf053b2842d91482b

instance Binary Metadata where
  put = putMetadata
  
  get = do
    magic' <- get
    version <- getWord16be
    doctype <- getWord8

    if magic' /= magic
      then fail "missing magic"
      else case (version, doctype) of
        (0, 0) -> getSimpleRedirect V0
        (0, 1) -> getMultiLevel V0
        (0, 2) -> getSimpleManifest
        (0, 3) -> getArchiveManifest V0
        (0, 4) -> getArchiveInternalRedirect
        (0, 5) -> getArchiveMetadataRedirect
        (0, 6) -> getSymbolicShortlink
        (1, 0) -> getSimpleRedirect V1
        (1, 3) -> getArchiveManifest V1
        vd     -> fail $ "unknown version/doctype " ++ show vd
    
parseMetadata :: BSL.ByteString -> Either T.Text Metadata
parseMetadata bs = case decodeOrFail bs of
  Left (_, off, e) -> Left $ T.concat [T.pack e, " at ", T.pack $ show off]
  Right(_, _, md)  -> Right md
                                                         
-----------------------------------------------------------------------------------------------
-- binary serialization
-----------------------------------------------------------------------------------------------

putMetadata :: Metadata -> Put
putMetadata md = do
  put magic
  
  case md of
    Manifest mes          -> putManifest mes
    SimpleRedirect hs tgt -> putSimpleRedirect hs tgt
    x                     -> error $ "can't put " ++ show x 

putSimpleRedirect :: [(HashType, BS.ByteString)] -> RedirectTarget -> Put
putSimpleRedirect hs tgt = do
  putWord16be 1 -- version 1
  putWord8 0    -- doctype "SimpleRedirect"
  putWord16be $ foldl' (.|.) 0 $ map flagBit $ [Hashes, FullKeys] 
  putHashes hs
  
  case tgt of
    RedirectKey (Just mime) uri -> putMime mime >> putUri uri
    x                           -> error $ "putSimpleRedirect: can't put " ++ show x

putUri :: URI -> Put
putUri uri = putWord16be (fromIntegral len) >> putLazyByteString bs where
  len = BSL.length bs
  bs  = encode uri

putMime :: T.Text -> Put
putMime mime
  | len > 255 = error "putMime: MIME type > 255 bytes"
  | otherwise = putWord8 (fromIntegral len) >> putByteString bs
  where
    len = BS.length bs
    bs  = encodeUtf8 mime

putHashes :: [(HashType, BS.ByteString)] -> Put
putHashes hs = do
  putWord32be $ foldl' (.|.) 0 $ map (hashTypeFlag . fst) hs
  mapM_ go [minBound..]
  where
    go ht = case lookup ht hs of
      Just bs -> if hashTypeSize ht == BS.length bs
                 then putByteString bs
                 else error "putHashes: invalid hash size"
      Nothing -> return ()

putManifestEntry :: (T.Text, Metadata) -> Put
putManifestEntry (name, md) = do
  putWord16be nameLen >> putByteString nameBytes
  putWord16be mdLen >> putLazyByteString mdBytes
  where
    mdLen     = fromIntegral $ BSL.length mdBytes
    mdBytes   = encode md
    nameLen   = fromIntegral $ BS.length nameBytes
    nameBytes = encodeUtf8 name

putManifest :: ManifestEntries -> Put
putManifest mes = do
  putWord16be 0 -- version 0
  putWord8 2    -- doctype "SimpleManifest"
  putWord32be (fromIntegral $ length mes)
  mapM_ putManifestEntry mes
