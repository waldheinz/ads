
module Properties ( testProperties ) where

import Test.Framework

import Properties.CHK
import Properties.Statistics
import Properties.Types

testProperties :: [Test]
testProperties =
  [ testGroup "CHK" chkTests
  , testGroup "stats" statsTests
  , testGroup "types" typeTests
  ]
  
