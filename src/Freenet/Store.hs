
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

import Freenet.Types

----------------------------------------------------------------
-- store types
----------------------------------------------------------------

data StorePersistable r f = SP
                            { storeSize :: Int
                            , storePut :: (DataFound f) => f -> Put
                            , storeGet :: (DataRequest r, DataFound f) => r -> Get f
                            }
data StoreRequest r f
     = ReadRequest  r (TMVar (Maybe f)) -- ^ the request and where to put the data when found
     | WriteRequest f                   -- ^ the data to put in the store

data StoreFile r f = StoreFile
                     { _sfThread :: ! ThreadId
                     , sfReqs    :: ! (TBQueue (StoreRequest r f))
                     , _sfHandle :: ! Handle
                     }

mkStoreFile :: (DataRequest r, DataFound f) => StorePersistable r f -> FilePath -> Int -> IO (StoreFile r f)
mkStoreFile sp fileName count = do
  rq <- newTBQueueIO 10
  
  let
    entrySize = 1 + storeSize sp
    doGet dr = do
      flags <- getWord8
      if flags == 1
        then storeGet sp dr
        else fail "empty slot"
    doPut df    = putWord8 1 >> storePut sp df
    isFree = getWord8 >>= (\flags -> return $ flags == 1)

    fileSize  = count * (fromIntegral entrySize)
  
  handle <- openBinaryFile fileName ReadWriteMode
  hSetFileSize handle $ fromIntegral fileSize

  let 
    getOffsets loc =
      let idx = (fromIntegral $ hash loc :: Word32) `rem` (fromIntegral count)
      in map (\i -> (fromIntegral i) * (fromIntegral entrySize)) [idx .. idx + 5]

    doRead dr bucket = go offsets where
      loc       = dataRequestLocation dr
      offsets   = getOffsets loc
      go []     = atomically $ putTMVar bucket Nothing -- no offsets left, terminate
      go (o:os) = do
--        print $ "checking read " ++ show o
        
        hSeek handle AbsoluteSeek o
        d <- BSL.hGet handle entrySize
        
        case runGetOrFail (doGet dr) d of
          Left  (_, _, _)  -> (print $ "nothing to read at " ++ show o) >> go os
          Right (_, _, df) -> if loc == dataFoundLocation df
                              then print ("found read at " ++ show o) >> (atomically $ putTMVar bucket $ Just df)
                              else print ("something else at " ++ show o) >>  go os
          
    doWrite df = go offsets where
      loc       = dataFoundLocation df
      offsets   = getOffsets loc
      go []     = print "no free slots"
      go (o:os) = do
        hSeek handle AbsoluteSeek o
        d <- BSL.hGet handle 1
        case runGetOrFail isFree d of
          Right (_, _, free) -> do
            if free
              then do
--                print $ "no valid data at " ++ show o ++ ": " ++ e
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
  
putData :: DataFound f => StoreFile r f -> f -> STM ()
putData sf df = writeTBQueue (sfReqs sf) (WriteRequest df)

getData :: (DataRequest r, DataFound f) => StoreFile r f -> r -> IO (Maybe f)
getData fs dr = do
  bucket <- newEmptyTMVarIO
  atomically $ writeTBQueue (sfReqs fs) (ReadRequest dr bucket)
  atomically $ takeTMVar bucket
