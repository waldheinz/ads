
module Main ( main ) where

import Sandbox

main :: IO ()
main = runSandbox $ do
  randomNode >> return ()
  
