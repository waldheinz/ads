
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Concurrent.STM
import Control.Monad ( void )
import qualified Data.Configurator as CFG
import Network ( withSocketsDo )

import Freenet as FN
import Freenet.URI as FU
import Logging as LOG
import Net
import Node as N
import Peers as P

main :: IO ()
main = withSocketsDo $ do
  cfg <- CFG.load [CFG.Required "configs/test.cfg"]

  LOG.initLogging $ CFG.subconfig "logging" cfg
  infoM "main" "Starting up..."
  
  ni <- getNodeInfo (CFG.subconfig "node" cfg)
  p <- atomically mkPeers
  fn <- FN.initFn (CFG.subconfig "freenet" cfg)
  
  nodeListen (CFG.subconfig "node.listen" cfg) ni p
  {-
  void $ forkIO $ N.connectNode ni ("127.0.0.1", 1234) $ \n -> do
    print n
  -}
  let
    uri = "CHK@gH-tDJQQeA0yLf9jGEPLsSZlJ-WKbY4wYFW8BogJRlo,x0sJ16zdebsuehrpvnyRepe9hpdIPJ9wDpUuf~wJ3XA,AAMA--8"
    
  case FU.parseUri uri of
    Left e -> error $ show e
    Right u -> void $ FN.fetchUri fn u >>= print
