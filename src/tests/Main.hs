
module Main ( main ) where

import Test.Framework

import Properties

main :: IO ()
main = defaultMain
       [ testGroup "properties" testProperties
       ]
