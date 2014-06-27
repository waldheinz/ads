
module Main ( main ) where

import Test.Framework

import Properties
import StoreTest

main :: IO ()
main = defaultMain
       [ testGroup "properties" testProperties
       , storeTests
       ]
