
{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}

module Freenet.Store (
  StoreFile, mkStoreFile, putData, getData,
  
  -- * things that go to the store
  StorePersistable(..)
  ) where

import Control.Concurrent ( forkIO, ThreadId )
import Control.Concurrent.STM
import Control.Monad ( forever )
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

data StoreRequest f
     = ReadRequest Key (TMVar (Maybe f)) -- ^ the request and where to put the data when found
     | WriteRequest f                    -- ^ the data to put in the store

data StoreFile f = StoreFile
                     { _sfThread :: ! ThreadId
                     , sfReqs    :: ! (TBQueue (StoreRequest f))
                     , _sfHandle :: ! Handle
                     }

logI :: String -> IO ()
logI m = infoM "freenet.store" m
  
mkStoreFile :: StorePersistable f => f -> FilePath -> Int -> IO (StoreFile f)
mkStoreFile sp fileName count = do
  rq <- newTBQueueIO 10
  
  let
    entrySize = 1 + storeSize sp
    doGet = do
      flags <- getWord8
      if flags == 1
        then storeGet
        else fail "empty slot"
    doPut df    = putWord8 1 >> storePut df
    isFree = getWord8 >>= (\flags -> return $ flags == 0)

    fileSize  = count * (fromIntegral entrySize)
  
  handle <- openBinaryFile fileName ReadWriteMode
  hSetFileSize handle $ fromIntegral fileSize

  let 
    getOffsets loc =
      let idx = (fromIntegral $ hash loc :: Word32) `rem` (fromIntegral count)
      in map (\i -> (fromIntegral i) * (fromIntegral entrySize)) [idx .. idx + 5]

    doRead loc bucket = go offsets where
      offsets   = getOffsets loc
      go []     = atomically $ putTMVar bucket Nothing -- no offsets left, terminate
      go (o:os) = do
        hSeek handle AbsoluteSeek o
        d <- BSL.hGet handle entrySize
        
        case runGetOrFail doGet d of
          Left  (_, _, _)  -> go os
          Right (_, _, df) -> if loc == dataBlockLocation df
                              then atomically $ putTMVar bucket $ Just df
                              else go os
          
    doWrite df = go offsets where
      loc       = dataBlockLocation df
      offsets   = getOffsets loc
      go []     = logI "no free slots in store"
      go (o:os) = do
        hSeek handle AbsoluteSeek o
        d <- BSL.hGet handle 1
        case runGetOrFail isFree d of
          Right (_, _, free) -> do
            if free
              then do
                hSeek handle AbsoluteSeek o
                BSL.hPut handle $ runPut $ doPut df
                print $ "written at " ++ show o
              else go os
          Left (_, _, e) -> error e
  
  tid <- forkIO $ forever $ do
    req <- atomically $ readTBQueue rq
    
    case req of
      ReadRequest dr bucket -> doRead dr bucket
      WriteRequest df       -> doWrite df

  return $ StoreFile tid rq handle
  
putData :: DataBlock f => StoreFile f -> f -> STM ()
putData sf df = writeTBQueue (sfReqs sf) (WriteRequest df)

getData :: DataBlock f => StoreFile f -> Key -> IO (Maybe f)
getData fs key = do
  bucket <- newEmptyTMVarIO
  atomically $ writeTBQueue (sfReqs fs) (ReadRequest key bucket)
  atomically $ takeTMVar bucket
