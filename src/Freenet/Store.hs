
module Freenet.Store (
  FileStore, mkFileStore, putData, getData
  ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( forever, void, when )
import Data.Binary
import Data.Binary.Get ( ByteOffset )
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable
import System.FilePath
import System.IO

import Freenet.Data
import Freenet.Types

data StoreRequest
     = ReadRequest DataRequest (TMVar (Maybe DataFound)) -- ^ the request and where to put the data when found
     | WriteRequest DataFound -- ^ the data to put in the store

data FileStore = FS
                 { _blockCount :: Int
                 , writeQueue :: TBQueue StoreRequest
                 , _chkStore   :: Handle
                 }

mkFileStore
  :: Int              -- ^ number of elements the store can hold
  -> FilePath         -- ^ directory holding the store
  -> IO FileStore
mkFileStore count dir = do
  wq <- newTBQueueIO 10

  let
    chkSize = 32837
    chkFileSize = count * chkSize
  
  chkHandle <- openBinaryFile (dir </> "chk-store") ReadWriteMode
  hSetFileSize chkHandle $ fromIntegral chkFileSize
  
  -- dispatch write requests to the file(s)
  void $ forkIO $ forever $ do
    req <- atomically $ readTBQueue wq
    
    let
      len     = chkSize -- BSL.length binary
      location = case req of
        ReadRequest (ChkRequest key _) _ -> key
        WriteRequest df                  -> dataFoundLocation df
      idx     = keyIndex count location
      offsets = map (\i -> (fromIntegral i) * (fromIntegral len)) [idx .. idx + 5]
      write _ []     = print "no free slot"
      write df (o:os) = do
        print $ "checking " ++ show o
        hSeek chkHandle AbsoluteSeek o
        pd <- BSL.hGetContents chkHandle
        
        case (decodeOrFail pd :: Either (BSL.ByteString, ByteOffset, String) (BSL.ByteString, ByteOffset, DataFound)) of
          Left  _ -> do
            hSeek chkHandle AbsoluteSeek o
            BSL.hPut chkHandle $ encode df
            print $ "written at " ++ show o
            
          Right (_, _, old) -> if old == df
                               then print "already there" >> return ()
                               else write df os

    
      handleRead [] _ bucket = atomically $ putTMVar bucket Nothing
      handleRead (o:os) key bucket = do
        hSeek chkHandle AbsoluteSeek o
        pd <- BSL.hGetContents chkHandle
        case (decodeOrFail pd :: Either (BSL.ByteString, ByteOffset, String) (BSL.ByteString, ByteOffset, DataFound)) of
          Left _ -> handleRead os key bucket -- ^ TODO couldn't we cancel reading here?
          Right (_, _, df) -> if key == dataFoundLocation df
                              then print "found in store" >> (atomically $ putTMVar bucket $ Just df)
                              else handleRead os key bucket
    
    when (fromIntegral len /= chkSize) $ error $ "size mismatch: " ++ (show (chkSize, len))
    
    case req of
      WriteRequest df                     -> write df offsets 
      ReadRequest (ChkRequest key _) buck -> handleRead offsets key buck
    
  return $ FS count wq chkHandle
  
putData :: FileStore -> DataFound -> STM ()
putData fs df = writeTBQueue (writeQueue fs) (WriteRequest df)

getData :: FileStore -> DataRequest -> IO (Maybe DataFound)
getData fs dr = do
  bucket <- newEmptyTMVarIO
  atomically $ writeTBQueue (writeQueue fs) (ReadRequest dr bucket)
  atomically $ takeTMVar bucket

keyIndex :: Int -> Key -> Word32
keyIndex cnt key = (fromIntegral $ hash key :: Word32) `rem` (fromIntegral cnt)

