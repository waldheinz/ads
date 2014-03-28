
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
  
  -- install signal handler for shutdown handling
  shutdown <- newTVarIO False
  void $ installHandler sigINT (Catch $ sigHandler shutdown) Nothing
  void $ installHandler sigTERM (Catch $ sigHandler shutdown) Nothing

  -- initialize logging
  appDir <- getAppUserDataDirectory "ads"
  cfg <- CFG.load [CFG.Required $ appDir </> "config"]
  LOG.initLogging $ CFG.subconfig "logging" cfg
  
  let
    fnConfig = (CFG.subconfig "freenet" cfg)
  
  infoM "main" "Starting up..."

  -- start our node
  ni <- getNodeInfo (CFG.subconfig "node" cfg)
  p <- atomically mkPeers
  fn <- FN.initFn fnConfig
  
  nodeListen (CFG.subconfig "node.listen" cfg) ni p
  
  -- start fproxy
  fproxyEnabled <- CFG.require fnConfig "fproxy.enabled"
  when fproxyEnabled $ do
    fpPort <- CFG.require fnConfig "fproxy.port"
    void $ forkIO $ Warp.run fpPort $ FP.fproxy fn

  -- wait for shutdown
  atomically $ readTVar shutdown >>= check
