
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void, when )
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as CFG
import Network ( withSocketsDo )
import Network.Wai.Handler.Warp as Warp
import System.Directory ( getAppUserDataDirectory )
import System.Environment ( getArgs )
import System.FilePath ( (</>) )
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

import Freenet as FN
import Freenet.Fproxy as FP
import Freenet.Rijndael as RD
import Logging as LOG
import Net
import Node
import RestApi

logE :: String -> IO ()
logE m = errorM "main" m

sigHandler :: TVar Bool -> IO ()
sigHandler s = do
  infoM "main" "shutdown on sigint/sigterm"
  atomically $ writeTVar s True

main :: IO ()
main = withSocketsDo $ do
  RD.initRijndael

  args <- getArgs
  appDir <- if null args then getAppUserDataDirectory "ads" else return (head args)
  
  -- install signal handler for shutdown handling
  shutdown <- newTVarIO False
  void $ installHandler sigINT (Catch $ sigHandler shutdown) Nothing
  void $ installHandler sigTERM (Catch $ sigHandler shutdown) Nothing
  
  -- initialize logging
  cfg <- CFG.load [CFG.Required $ appDir </> "config"]
  LOG.initLogging $ CFG.subconfig "logging" cfg
  
  let
    fnConfig = (CFG.subconfig "freenet" cfg)
  
  infoM "main" "Starting up..."

  -- start Freenet
  fn <- FN.initFn fnConfig

  -- start our node
  mi <- eitherDecode <$> BSL.readFile (appDir </> "identity")
  
  node <- case mi of
    Left e -> logE ("error reading node identity: " ++ e) >> error "can't continue"
    Right ni -> do
      infoM "main" $ show mi
      n <- mkNode ni fn
      initPeers n tcpConnect appDir
      nodeListen (CFG.subconfig "node.listen" cfg) n
      return n

  -- start HTTP Server
  void $ forkIO $ Warp.run 8080 (restApi node)

  -- start fproxy
  fproxyEnabled <- CFG.require fnConfig "fproxy.enabled"
  when fproxyEnabled $ do
    fpPort <- CFG.require fnConfig "fproxy.port"
    void $ forkIO $ Warp.run fpPort $ FP.fproxy node

  -- wait for shutdown
  atomically $ readTVar shutdown >>= check
