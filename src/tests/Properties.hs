
module Properties ( testProperties ) where

import Test.Framework

import Properties.Statistics
import Properties.Types

testProperties :: [Test]
testProperties =
  [ testGroup "types" typeTests
  , testGroup "stats" statsTests
  ]
  
