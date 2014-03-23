
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Concurrent.STM
import qualified Data.Configurator as CFG
import Network ( withSocketsDo )
import Network.Wai.Handler.Warp as Warp

import Freenet as FN
import Freenet.Fproxy as FP
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

  Warp.run 8081 (FP.fproxy fn)
  
  {-
  void $ forkIO $ N.connectNode ni ("127.0.0.1", 1234) $ \n -> do
    print n
  
  let
    uri = "CHK@WwKhAnEKnmJMiHZ-Cs7YTJ8jRS505-WLoVoTK4ZadVg,8-kP5bnx9tpQoMEXm1kQibnblvrQh0hi8-7Bii4uDrY,AAMC--8/088748722_Bob7z854_123_12lo.jpg"
    
  case FU.parseUri uri of
    Left e -> error $ show e
    Right u -> FN.fetchUri fn u >>= \r -> case r of
      Left e -> error $ T.unpack e
      Right bs -> BSL.writeFile "fetched" bs
-}
