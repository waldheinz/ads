
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Store (
  StoreFile, mkStoreFile, shutdownStore,
  putData, getData,
  
  -- * things that go to the store
  StorePersistable(..)
  ) where

import qualified Control.Concurrent.Lock as Lock
import Control.Concurrent.STM
import Control.Monad ( replicateM, unless, void, when )
import Data.Aeson
import Data.Binary
import Data.Binary.Get ( runGetOrFail )
import Data.Binary.Put ( runPut )
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable
import System.Directory ( renameFile )
import System.IO
import System.IO.Error ( catchIOError )
import System.Log.Logger
import System.Random ( randomRIO )

import Freenet.Types
import Statistics
import Types

----------------------------------------------------------------
-- store types
----------------------------------------------------------------

data StoreFile f = StoreFile
                     { sfLock        :: ! Lock.Lock -- ^ for accessing the handle
                     , sfFileName    :: ! FilePath
--                     , sfHandles     :: ! (TVar [Handle])
                     , sfEntrySize   :: ! Int
                     , sfEntryCount  :: ! Int
                     , sfReads       :: ! (TVar Word64) -- ^ total number of reads
                     , sfReadSuccess :: ! (TVar Word64) -- ^ number of successful reads
                     , sfHistogram   :: ! THistogram
                     , sfReadOffset  :: ! (Integer -> IO (Maybe f)) -- ^ try to read data from the given offset
                     , sfWriteOffset :: ! (Integer -> f -> IO ())   -- ^ actually write data to the given offset
                     }

instance ToStateJSON (StoreFile f) where
  toStateJSON sf = do
    r <- readTVar $ sfReads sf
    s <- readTVar $ sfReadSuccess sf
    h <- toStateJSON $ sfHistogram sf
    
    return $ object
      [ "entrySize"    .= sfEntrySize sf
      , "capacity"     .= sfEntryCount sf
      , "readRequests" .= r
      , "readSuccess"  .= s
      , "histogram"    .= h
      ]

logI :: String -> IO ()
logI m = infoM "freenet.store" m

mkStoreFile :: StorePersistable f => f -> FilePath -> Int -> IO (StoreFile f)
mkStoreFile sp fileName count = do
  
  let
    entrySize = 1 + storeSize sp
    fileSize  = count * (fromIntegral entrySize)
  
  handle <- openBinaryFile fileName ReadWriteMode
  hSetFileSize handle $ fromIntegral fileSize
  
  rds  <- newTVarIO 0
  scs  <- newTVarIO 0
  lck  <- Lock.new
 -- hnds <- newTVarIO handles
  (needScan, hist) <- readStats fileName
  
  let sf = StoreFile lck fileName entrySize count rds scs hist
           (readOffset sf handle) (writeOffset sf handle)
           
  when needScan $ void $ scanStore sf
  
  return sf

readStats :: FilePath -> IO (Bool, THistogram)
readStats fp = do
  hist <- catchIOError doRead handler >>= \(ns, x) -> do
    h <- atomically $ thawHistogram x
    return (ns, h)
    
  catchIOError (renameFile fname $ fname ++ ".bak") $ const $ return ()
  return hist
  where
    fname = fp ++ "-histogram"
    doRead = decodeFile fname >>= \x -> return (False, x)
    handler e = do
      logI $ "error reading histogram: " ++ show e
      return $ (True, mkHistogram 256)
  
shutdownStore :: StoreFile f -> IO ()
shutdownStore sf = do
  logI $ "shutting down store"

--  hClose $ sfHandle sf
  hist <- atomically $ freezeHistogram (sfHistogram sf)
  encodeFile (sfFileName sf ++ "-histogram") hist
  
scanStore :: (DataBlock f) => StoreFile f -> IO ()
scanStore sf = do mapM_ checkOffset offsets
  where
    ecount  = fromIntegral $ sfEntryCount sf
    esize   = fromIntegral $ sfEntrySize sf
    offsets = [0, esize .. (ecount - 1) * esize] :: [Integer]

    checkOffset o = do
      r <- Lock.with (sfLock sf) $ sfReadOffset sf o
      
      case r of
        Nothing -> return ()
        Just df -> atomically $ histInc (sfHistogram sf) $ nodeIdToDouble $ keyToNodeId $ dataBlockLocation df

locOffsets :: StoreFile f -> Key -> [Integer]
locOffsets sf loc = map (\i -> (fromIntegral i `rem` (fromIntegral count)) * (fromIntegral entrySize)) [idx .. idx + 5]
  where
    idx = (fromIntegral $ hash loc :: Word32) `rem` (fromIntegral count)
    count = sfEntryCount sf
    entrySize = sfEntrySize sf

-- | Actually write data to the given offset and update stats.
writeOffset :: StorePersistable f => StoreFile f -> Handle -> Integer -> f -> IO ()
writeOffset sf handle o df = do
  hSeek handle AbsoluteSeek o
  BSL.hPut handle $ runPut doPut
  logI $ (show loc) ++ " written at " ++ show o
  atomically $ histInc (sfHistogram sf) $ nodeIdToDouble $ keyToNodeId loc
  where
    loc = dataBlockLocation df
    doPut = putWord8 1 >> storePut df
    
putData :: StorePersistable f => StoreFile f -> f -> IO ()
putData sf df = Lock.with (sfLock sf) $ go [] (locOffsets sf loc) where
  loc = dataBlockLocation df
  -- there are no free slots, we must decide if and which we want to overwrite
  go olds [] = do
    p <- randomRIO (0, 1) :: IO Double
    unless (p > 0.1) $ do -- TODO: use something clever instead of 0.1, maybe try to match the incoming req. distribution?
      -- we want to overwrite
      (o, loc') <- randomRIO (0, length olds - 1) >>= return . (olds !!)
      atomically $ histDec (sfHistogram sf) $ nodeIdToDouble $ keyToNodeId loc'
      sfWriteOffset sf o df
    
  -- we still have some candidate slots which are possibly empty, check them
  go olds (o:os) = sfReadOffset sf o >>= \old -> case old of
    Nothing  -> sfWriteOffset sf o df            -- place data in previously empty slot
    Just df' -> let loc' = dataBlockLocation df' -- we're done if the data is already there
                in unless (loc == loc') $ go ((o, loc'):olds) os

readOffset :: StorePersistable f => StoreFile f -> Handle -> Integer -> IO (Maybe f)
readOffset sf handle offset = do
  hSeek handle AbsoluteSeek offset
  d <- BSL.hGet handle $ sfEntrySize sf
  
  case runGetOrFail doGet d of
    Left (_, _, _)   -> return Nothing
    Right (_, _, df) -> return $ Just df
    
  where
    doGet = getWord8 >>= \flags -> case flags of
      1 -> storeGet
      _ -> fail "empty slot"
      
getData :: StorePersistable f => StoreFile f -> Key -> IO (Maybe f)
getData sf key = Lock.with (sfLock sf) $ doRead key >>= \result -> countRead result >> return result where
  
  countRead r = atomically $ modifyTVar' (sfReads sf) (+1) >> case r of
    Nothing -> return ()
    Just _  -> modifyTVar' (sfReadSuccess sf) (+1)

  doRead loc = go (locOffsets sf loc) where
    go []     = return $ Nothing -- no offsets left, terminate
    go (o:os) = do
      d <- sfReadOffset sf o
      
      case d of
        Nothing -> go os
        Just df -> if loc == dataBlockLocation df
                   then return $ Just df
                   else go os
