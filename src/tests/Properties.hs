
module Properties ( testProperties ) where

import Test.Framework

import Properties.Types

testProperties :: [Test]
testProperties = [testGroup "types" typeTests]
