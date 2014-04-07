
{-# LANGUAGE OverloadedStrings #-}

module Freenet.SplitFile (
  SplitFile(..), SplitFileSegment(..),
  fetchSplitFile
  ) where

import Control.Concurrent.Async ( mapConcurrently )
import Data.Word
import qualified Data.ByteString.Lazy as BSL
import Data.Either ( partitionEithers )
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import System.Log.Logger

import Freenet.Compression
import Freenet.Mime
import Freenet.URI
import Types

data SplitFileSegment
  = SplitFileSegment
    { sfUri   :: ! URI   -- ^ the URI where this segment can be fetched
    , sfsData :: ! Bool  -- ^ True if this is a data block, false if it's a check block
    }
    deriving ( Eq, Ord, Show )

data SplitFile = SplitFile
                 { sfCompression    :: CompressionCodec   -- ^ the compression codec used by this splitfile
                 , sfCompressedSize :: Word64             -- ^ size of compressed data, equals original size if not compressed
                 , sfOriginalSize   :: Word64             -- ^ size of original data before compression was applied
                 , sfSegments       :: [SplitFileSegment] -- ^ the segments this split consists of
                 , sfMime           :: Maybe Mime         -- ^ MIME type of the target data
                 } deriving ( Eq, Ord, Show )

logI :: String -> IO ()
logI m = infoM "freenet.splitfile" m

fetchUris :: (UriFetch a) => a -> [URI] -> IO [(URI, Either T.Text BSL.ByteString)]
fetchUris fn uris = do
  result <- mapConcurrently (getUriData fn) uris
  return $ zip uris result

fetchSplitFile :: (UriFetch a) => a -> SplitFile -> IO (Either T.Text BSL.ByteString)
fetchSplitFile fn (SplitFile comp dlen _ segs _) = do -- TODO: we're not returning the MIME and ignoring the original length
  logI $ "fetch split file"
  
  let
    -- TODO: use FEC check blocks as well
    segUris = catMaybes $ map (\(SplitFileSegment uri isd) -> if isd then Just uri else Nothing) segs

  ds <- fetchUris fn segUris
  
  -- see if everything could be fetched and try to decrypt
  let
    (es, bs) = partitionEithers (map snd ds)

  if (not . null) es
    then return $ Left $ T.intercalate ", " es
    else decompress comp $ BSL.take (fromIntegral dlen) $ BSL.concat bs
