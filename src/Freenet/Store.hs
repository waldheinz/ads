
{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}

module Freenet.Store (
  StoreFile, mkStoreFile, putData, getData,
  
  -- * things that go to the store
  StorePersistable(..)
  ) where

import qualified Control.Concurrent.Lock as Lock
import Data.Binary
import Data.Binary.Get ( runGetOrFail )
import Data.Binary.Put ( runPut )
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable
import System.IO
import System.Log.Logger

import Freenet.Types

----------------------------------------------------------------
-- store types
----------------------------------------------------------------

data StoreFile f = StoreFile
                     { sfLock       :: ! Lock.Lock -- ^ for accessing the handle
                     , sfHandle     :: ! Handle
                     , sfEntrySize  :: ! Int
                     , sfEntryCount :: ! Int
                     }

logI :: String -> IO ()
logI m = infoM "freenet.store" m
  
mkStoreFile :: StorePersistable f => f -> FilePath -> Int -> IO (StoreFile f)
mkStoreFile sp fileName count = do
  
  let
    entrySize = 1 + storeSize sp
    fileSize  = count * (fromIntegral entrySize)
  
  handle <- openBinaryFile fileName ReadWriteMode
  hSetFileSize handle $ fromIntegral fileSize
    
  lck <- Lock.new
  return $ StoreFile lck handle entrySize count

locOffsets :: StoreFile f -> Key -> [Integer]
locOffsets sf loc = map (\i -> (fromIntegral i `rem` (fromIntegral count)) * (fromIntegral entrySize)) [idx .. idx + 5]
  where
    idx = (fromIntegral $ hash loc :: Word32) `rem` (fromIntegral count)
    count = sfEntryCount sf
    entrySize = sfEntrySize sf
    
putData :: StorePersistable f => StoreFile f -> f -> IO ()
putData sf df = Lock.with (sfLock sf) $ doWrite where
  handle = sfHandle sf
  doPut  = putWord8 1 >> storePut df
  isFree = getWord8 >>= (\flags -> return $ flags == 0)

  doWrite = go offsets where
      loc       = dataBlockLocation df
      offsets   = locOffsets sf loc
      go []     = logI "no free slots in store"
      go (o:os) = do
        hSeek handle AbsoluteSeek o
        d <- BSL.hGet handle 1
        case runGetOrFail isFree d of
          Right (_, _, free) -> do
            if free
              then do
                hSeek handle AbsoluteSeek o
                BSL.hPut handle $ runPut doPut
                print $ "written at " ++ show o
              else go os
          Left (_, _, e) -> error e
  

getData :: StorePersistable f => StoreFile f -> Key -> IO (Maybe f)
getData fs key = Lock.with (sfLock fs) $ doRead key where
  handle = sfHandle fs
  doGet = do
    flags <- getWord8
    if flags == 1
      then storeGet
      else fail "empty slot"

  doRead loc = go offsets where
    offsets   = locOffsets fs loc
    go []     = return $ Nothing -- no offsets left, terminate
    go (o:os) = do
      hSeek handle AbsoluteSeek o
      d <- BSL.hGet handle $ sfEntrySize fs
        
      case runGetOrFail doGet d of
        Left  (_, _, _ ) -> go os
        Right (_, _, df) -> if loc == dataBlockLocation df
                            then return $ Just df
                            else go os
    
