
module StoreTest ( storeTests ) where

import           Data.ByteString as BS
import           System.IO.Temp
import qualified Test.Framework as TF
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import           Freenet.Store
import Freenet.Types

storeTests :: TF.Test
storeTests = TF.testGroup "store"
             [ testCase "create store" testCreate
             ]

testCreate :: Assertion
testCreate = withSystemTempDirectory "ads-test" $ \tmpdir -> do
  sf <- mkStoreFile (tmpdir ++ "dummy-store") 1000 :: IO (StoreFile DummyStorable)
  return ()

dummySize :: Int
dummySize = 12345

newtype DummyStorable = DS BS.ByteString

instance DataBlock DummyStorable where
  

instance StorePersistable DummyStorable where
  storeSize _ = dummySize
