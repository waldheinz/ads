
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void, when )
import qualified Data.Configurator as CFG
import Network ( withSocketsDo )
import Network.Wai.Handler.Warp as Warp
import System.Directory ( getAppUserDataDirectory )
import System.FilePath ( (</>) )
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

import Freenet as FN
import Freenet.Fproxy as FP
import Freenet.Rijndael as RD
import Logging as LOG
import Net
import Node as N
import Peers as P

sigHandler :: TVar Bool -> IO ()
sigHandler s = do
  infoM "main" "shutdown on sigint/sigterm"
  atomically $ writeTVar s True

main :: IO ()
main = withSocketsDo $ do
  RD.initRijndael

  shutdown <- newTVarIO False

  void $ installHandler sigINT (Catch $ sigHandler shutdown) Nothing
  void $ installHandler sigTERM (Catch $ sigHandler shutdown) Nothing
  
  appDir <- getAppUserDataDirectory "ads"
  cfg <- CFG.load [CFG.Required $ appDir </> "config"]

  let
    fnConfig = (CFG.subconfig "freenet" cfg)
  
  LOG.initLogging $ CFG.subconfig "logging" cfg
  infoM "main" "Starting up..."
  
  ni <- getNodeInfo (CFG.subconfig "node" cfg)
  p <- atomically mkPeers
  fn <- FN.initFn fnConfig
  
  nodeListen (CFG.subconfig "node.listen" cfg) ni p
  
  -- fproxy
  fproxyEnabled <- CFG.require fnConfig "fproxy.enabled"
  when fproxyEnabled $ do
    fpPort <- CFG.require fnConfig "fproxy.port"
    void $ forkIO $ Warp.run fpPort $ FP.fproxy fn

  atomically $ readTVar shutdown >>= check
    
    
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
