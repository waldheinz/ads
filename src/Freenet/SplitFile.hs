
{-# LANGUAGE OverloadedStrings #-}

module Freenet.SplitFile (
  SplitFile(..), SplitFileSegment(..),
  fetchSplitFile
  ) where

import qualified Codec.FEC as FEC
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Exception ( ErrorCall, catch )
import Control.Monad ( replicateM_ )
import Data.Word
import qualified Data.ByteString.Lazy as BSL
import Data.Function ( on )
import Data.List ( sortBy )
import qualified Data.Text as T
import System.Log.Logger
import System.Random ( newStdGen, randomR )

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

-- |
-- Fetch the contents of a SplitFile. A SplitFile consists of @k@
-- data blocks plus a number for FEC check blocks. We accomplish this by
-- fetching a random subset of @k@ blocks, and then run the FEC codec to
-- reassemble the original data. 
fetchSplitFile :: (UriFetch a) => a -> SplitFile -> IO (Either T.Text BSL.ByteString)
fetchSplitFile fn (SplitFile comp dlen _ segs _) = do -- TODO: we're not returning the MIME and ignoring the original length
  
  let
    total = length segs
    k     = length $ filter (\(SplitFileSegment _ isd) -> isd) segs -- # of primary blocks
    blist = zip [0..] $ map (\(SplitFileSegment uri _) -> uri) segs

  logI $ "fetch split file " ++ show total ++ "  " ++ show k
  
  todo    <- newTVarIO blist
  done    <- newTVarIO []
  rng     <- newStdGen >>= newTVarIO
  running <- newTVarIO k
  
  -- try to fetch k randomly chosen blocks
  let
    download = do
      -- choose block to fetch
      next <- atomically $ do
        todo'    <- readTVar todo
        done'    <- readTVar done
        rng'     <- readTVar rng
      
        if null todo' || length done' == k
          then return Nothing
          else let (idx, rng'') = randomR (0, length todo' - 1) rng'
                   (ys, zs) = splitAt idx todo'
               in do
                 writeTVar todo $ ys ++ (tail zs)
                 writeTVar rng rng''
                 return $ Just $ head zs
                 
      case next of
        Nothing         -> return ()
        Just (idx, uri) -> do
          result <- getUriData fn uri
          case result of
            Left _    -> return () 
            Right (blk, _) -> atomically $ modifyTVar' done ((:) (idx, blk))
              
          download

  replicateM_ k $ forkIO $ download >> (atomically $ modifyTVar' running pred)
  
  -- wait until k blocks have been downloaded

  fetched <- atomically $ do
    done'    <- readTVar done
    if length done' >= k
      then return $ Right (sortBy (compare `on` fst) done')
      else do
        running' <- readTVar running
        todo'    <- readTVar todo

        if length done' + length todo' + running' >= k
          then retry
          else return $ Left $ "could not download enough blocks " `T.append` (T.pack $ show (length done', length todo', running'))

  case fetched of
    Left e   -> return $ Left e
    Right bs -> do
      fec <- catch
             (return $ Right $ FEC.decode (FEC.fec k total) bs)
             (\e -> return $ Left $ T.pack $ show ( e :: ErrorCall))
             
      case fec of
        Left e -> return $ Left e
        Right bs'' -> decompress comp $ BSL.take (fromIntegral dlen) $ BSL.fromChunks bs''
