
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
    uri = "CHK@WwKhAnEKnmJMiHZ-Cs7YTJ8jRS505-WLoVoTK4ZadVg,8-kP5bnx9tpQoMEXm1kQibnblvrQh0hi8-7Bii4uDrY,AAMC--8/088748722_Bob7z854_123_12lo.jpg"
    
  case FU.parseUri uri of
    Left e -> error $ show e
    Right u -> void $ FN.fetchUri fn u
