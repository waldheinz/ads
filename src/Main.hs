
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM
import Control.Monad ( void )
import qualified Data.Configurator as CFG
import Network ( withSocketsDo )

import Logging as LOG
import Net
import Node as N
import Peers as P

main :: IO ()
main = withSocketsDo $ do
  cfg <- CFG.load [CFG.Required "configs/test.cfg"]

  LOG.initLogging $ CFG.subconfig "logging" cfg
  infoM "main" "Starting up..."

  let
    ni = NodeInfo 0.5

  p <- atomically mkPeers

  nodeListen (CFG.subconfig "node.listen" cfg) ni p
  
  void $ forkIO $ N.connectNode ni ("127.0.0.1", 1234) $ \n -> do
    print n

  threadDelay (1000 * 1000 * 100)
