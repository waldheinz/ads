
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Store (
  StoreFile, mkStoreFile, putData, getData,
  
  -- * things that go to the store
  StorePersistable(..)
  ) where

import qualified Control.Concurrent.Lock as Lock
import Control.Concurrent.STM
import Control.Monad ( void )
import Data.Aeson
import Data.Binary
import Data.Binary.Get ( runGetOrFail )
import Data.Binary.Put ( runPut )
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable
import System.IO
import System.Log.Logger

import Freenet.Types
import Types

----------------------------------------------------------------
-- store types
----------------------------------------------------------------

data StoreFile f = StoreFile
                     { sfLock        :: ! Lock.Lock -- ^ for accessing the handle
                     , sfHandle      :: ! Handle
                     , sfEntrySize   :: ! Int
                     , sfEntryCount  :: ! Int
                     , sfReads       ::   TVar Word64 -- ^ total number of reads
                     , sfReadSuccess ::   TVar Word64 -- ^ number of successful reads
                     }

instance ToStateJSON (StoreFile f) where
  toStateJSON sf = do
    r <- readTVar $ sfReads sf
    s <- readTVar $ sfReadSuccess sf
    return $ object
      [ "entrySize"    .= sfEntrySize sf
      , "capacity"     .= sfEntryCount sf
      , "readRequests" .= r
      , "readSuccess"  .= s
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
  
  rds <- newTVarIO 0
  scs <- newTVarIO 0
  lck <- Lock.new
  
  return $ StoreFile lck handle entrySize count rds scs

locOffsets :: StoreFile f -> Key -> [Integer]
locOffsets sf loc = map (\i -> (fromIntegral i `rem` (fromIntegral count)) * (fromIntegral entrySize)) [idx .. idx + 5]
  where
    idx = (fromIntegral $ hash loc :: Word32) `rem` (fromIntegral count)
    count = sfEntryCount sf
    entrySize = sfEntrySize sf
    
putData :: StorePersistable f => StoreFile f -> f -> IO ()
putData sf df = void $ putData' sf df

putData' :: StorePersistable f => StoreFile f -> f -> IO (Maybe f)
putData' sf df = Lock.with (sfLock sf) $ doWrite where
  handle = sfHandle sf
  doPut  = putWord8 1 >> storePut df
  loc = dataBlockLocation df

  doGet = do
    flags <- getWord8
    if flags == 1
      then storeGet
      else fail "empty slot"

  writeAt o = do
    hSeek handle AbsoluteSeek o
    BSL.hPut handle $ runPut doPut
    logI $ (show loc) ++ " written at " ++ show o

  doWrite = go $ locOffsets sf loc where
      go []     = {- logI "no free slots in store" >> -} return Nothing
      go (o:os) = do
        hSeek handle AbsoluteSeek o
        d <- BSL.hGet handle $ sfEntrySize sf
       
        case runGetOrFail doGet d of
          Left  (_, _, _ ) -> writeAt o >> return Nothing
          Right (_, _, df') -> if loc == dataBlockLocation df'
                              then logI ((show loc) ++ " already in store") >> (return $ Just df')
                              else go os
  
getData :: StorePersistable f => StoreFile f -> Key -> IO (Maybe f)
getData fs key = Lock.with (sfLock fs) $ doRead key >>= \result -> countRead result >> return result where
  countRead r = atomically $ modifyTVar' (sfReads fs) (+1) >> case r of
    Nothing -> return ()
    Just _  -> modifyTVar' (sfReadSuccess fs) (+1)
    
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
