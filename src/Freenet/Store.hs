
module Freenet.Store (
  FileStore, mkFileStore, blockCount
  ) where

import Types

data FileStore a = FS
                 { blockCount :: Int
                 }

mkFileStore :: (Block a)
  => a         -- ^ can be undefined
  => Int       -- ^ number of elements the store can hold
  -> FilePath  -- ^ file holding the store
  -> IO (FileStore a)
mkFileStore blk count file = do
  let bs = size blk
  return $ FS count
  
