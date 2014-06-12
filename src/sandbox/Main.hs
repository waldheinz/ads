
module Main ( main ) where

import Sandbox

main :: IO ()
main = runSandbox $ do
  n1 <- randomNode
  n2 <- randomNode
  n1 `addPeer` n2
  delay 10000000
