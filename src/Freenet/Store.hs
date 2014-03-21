
module Freenet.Store (
  FileStore, mkFileStore, putData
  ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( forever, forM_, void, when )
import Data.Binary
import Data.Binary.Get ( ByteOffset )
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable
import System.FilePath
import System.IO

import Freenet.Data
import Freenet.Types

data FileStore = FS
                 { blockCount :: Int
                 , writeQueue :: TBQueue DataFound
                 , chkStore   :: Handle
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
    df <- atomically $ readTBQueue wq
    let
      binary  = encode df
      len     = BSL.length binary
      idx     = keyIndex count $ dataFoundLocation df
      offsets = map (\i -> (fromIntegral i) * (fromIntegral len)) [idx .. idx + 5]
      write []     = print "no free slot"
      write (o:os) = do
        print $ "checking " ++ show o
        hSeek chkHandle AbsoluteSeek o
        pd <- BSL.hGetContents chkHandle
        
        case (decodeOrFail pd :: Either (BSL.ByteString, ByteOffset, String) (BSL.ByteString, ByteOffset, DataFound)) of
          Left  _ -> do
            hSeek chkHandle AbsoluteSeek o
            BSL.hPut chkHandle binary
            print $ "written at " ++ show o
            
          Right (_, _, old) -> if old == df
                               then print "already there" >> return ()
                               else write os
      
    when (fromIntegral len /= chkSize) $ error $ "size mismatch: " ++ (show (chkSize, len))
    write offsets
    
  return $ FS count wq chkHandle
  
putData :: FileStore -> DataHandler
putData fs = writeTBQueue (writeQueue fs)

keyIndex :: Int -> Key -> Word32
keyIndex cnt key = (fromIntegral $ hash key :: Word32) `rem` (fromIntegral cnt)

