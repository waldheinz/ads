
module Freenet.Store (
  FileStore, mkFileStore, putData, getData
  ) where

import Control.Concurrent ( forkIO, ThreadId )
import Control.Concurrent.STM
import Control.Monad ( forever, void, when )
import Data.Binary
import Data.Binary.Get ( ByteOffset, runGetOrFail )
import Data.Binary.Put ( runPut )
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable
import System.FilePath
import System.IO

import Freenet.Data
import Freenet.Types

data StoreRequest
     = ReadRequest DataRequest (TMVar (Maybe DataFound)) -- ^ the request and where to put the data when found
     | WriteRequest DataFound                            -- ^ the data to put in the store

data StoreFile = StoreFile
                 { sfName   :: ! String
                 , sfThread :: ! ThreadId
                 , sfReqs   :: ! (TBQueue StoreRequest)
                 , sfHandle :: ! Handle
                 }

mkStoreFile :: FilePath -> Int -> Int -> String -> IO StoreFile
mkStoreFile dir count entrySize storeName = do
  rq <- newTBQueueIO 10
  
  let
    fileSize  = count * (fromIntegral entrySize)
    fileName  = dir </> storeName
  
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
        
        case runGetOrFail (storePersistGet storeName) d of
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
        d <- BSL.hGet handle entrySize
        case runGetOrFail (storePersistGet storeName) d of
          Left (_, _, e) -> do
            print $ "no valid data at " ++ show o ++ ": " ++ e
            hSeek handle AbsoluteSeek o
            BSL.hPut handle $ runPut $ storePersistPut df
            print $ "written at " ++ show o
          Right (_, _, df') -> if loc == dataFoundLocation df'
                               then print "already there"
                               else print "skip collision" >> go os
  
  tid <- forkIO $ forever $ do
    req <- atomically $ readTBQueue rq
    
    case req of
      ReadRequest dr bucket -> doRead dr bucket
      WriteRequest df       -> doWrite df

  return $ StoreFile storeName tid rq handle
  
data FileStore = FS
                 { _blockCount :: ! Int
                 , fsDir       :: ! FilePath
                 , fsChk       :: ! StoreFile
                 }

mkFileStore
  :: Int              -- ^ number of elements the store can hold
  -> FilePath         -- ^ directory holding the store
  -> IO FileStore
mkFileStore count dir = do
  chk <- mkStoreFile dir count (32 + 36 + 32768) "store-chk"
  return $ FS count dir chk
  
putData :: FileStore -> DataFound -> STM ()
putData fs df = do
  let sf = case storePersistFile df of
        "store-chk" -> fsChk fs
        x           -> error $ "no store for " ++ x

  writeTBQueue (sfReqs sf) (WriteRequest df)
  
getData :: FileStore -> DataRequest -> IO (Maybe DataFound)
getData fs dr@(ChkRequest _ _) = do
  bucket <- newEmptyTMVarIO
  atomically $ writeTBQueue (sfReqs $ fsChk fs) (ReadRequest dr bucket)
  atomically $ takeTMVar bucket
