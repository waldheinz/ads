
module Freenet.Bucket (
  MemoryBucket, newMemoryBucket, waitBucketTimeout,
  putBucket
  ) where

import Control.Concurrent.STM
import Control.Monad ( unless )

-- |
-- Someone who is interested in a bucket's contents.
type BucketReader d = MemoryBucket d -> STM ()

-- |
-- A bucket which is held in RAM.
data MemoryBucket d = MemoryBucket
                      { mbContent     :: ! (TMVar d)
                      , mbCleanup     :: MemoryBucket d -> STM ()
                      , mbReaderCount :: TVar Int
                      }

instance Eq (MemoryBucket d) where
  (==) mb1 mb2 = mbContent mb1 == mbContent mb2

-- |
-- Creates a new, empty memory bucket.
newMemoryBucket :: (MemoryBucket d -> STM ()) -> BucketReader d -> STM (MemoryBucket d)
newMemoryBucket cleanup reader = do
  c   <- newEmptyTMVar
  cnt <- newTVar 1
  let b = MemoryBucket c cleanup cnt
  reader b
  release b
  return b

release :: MemoryBucket d -> STM ()
release mb = do
  cnt <- readTVar $ mbReaderCount mb
  let cnt' = cnt - 1
  if cnt' <= 0
    then mbCleanup mb mb
    else writeTVar (mbReaderCount mb) cnt'

-- |
-- Wait for data to be put in the bucket, and
-- get this data.
waitBucketTimeout :: MemoryBucket d -> IO (Maybe d)
waitBucketTimeout mb = do
  timeout <- registerDelay (1000 * 1000 * 10)
  atomically $ modifyTVar' (mbReaderCount mb) (+1)
  
  result <- atomically $ do
    empty <- isEmptyTMVar $ mbContent mb
    if empty
      then do
        to <- readTVar timeout
        if to
          then return Nothing
          else retry
      else do
        x <- readTMVar (mbContent mb)
        release mb
        return $ Just x

  return result
  
-- |
-- Puts data into this bucket. This function will
-- error if there is already data in the bucket.
putBucket :: MemoryBucket d -> d -> STM ()
putBucket mb d = do
  success <- tryPutTMVar (mbContent mb) d
  unless success $ error "putBucket: put to non-empty bucket"
  
