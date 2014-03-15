
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM
import qualified Data.Configurator as CFG
import Network ( withSocketsDo )

import Net
import Node as N
import Peers as P


main :: IO ()
main = withSocketsDo $ do
  cfg <- CFG.load [CFG.Required "configs/test.cfg"]

  let
    ni = NodeInfo 0.5

  p <- atomically mkPeers

  nodeListen (CFG.subconfig "node.listen" cfg) p

  threadDelay (1000 * 500)

  forkIO $ N.connectNode ni ("127.0.0.1", 1234) $ \n -> do
    print n

  threadDelay (1000 * 1000 * 100)
