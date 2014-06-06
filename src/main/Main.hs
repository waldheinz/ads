
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( unless, void )
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as CFG
import Network ( withSocketsDo )
import Network.Wai.Handler.Warp as Warp
import System.Directory ( createDirectoryIfMissing, doesFileExist, getAppUserDataDirectory )
import System.Environment ( getArgs )
import System.FilePath ( (</>) )
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import System.Random ( getStdRandom )

import Paths_ads

import Freenet as FN
import Freenet.Fproxy as FP
import Freenet.Rijndael as RD
import Logging as LOG
import Net
import Node
import RestApi
import Types

logI :: String -> IO ()
logI = infoM "main"

logE :: String -> IO ()
logE = errorM "main"

sigHandler :: TVar Bool -> IO ()
sigHandler s = do
  infoM "main" "shutdown on sigint/sigterm"
  atomically $ writeTVar s True

main :: IO ()
main = withSocketsDo $ do
  infoM "main" "Starting up..."
  
  RD.initRijndael

  -- find and maybe create out home directory
  args <- getArgs
  appDir <- if null args then getAppUserDataDirectory "ads" else return (head args)
  createDirectoryIfMissing True appDir
  
  -- install signal handler for shutdown handling
  shutdown <- newTVarIO False
  void $ installHandler sigINT (Catch $ sigHandler shutdown) Nothing
  void $ installHandler sigTERM (Catch $ sigHandler shutdown) Nothing

  -- load (and maybe create) configuration file
  let
    cfgFile  = appDir </> "config"
    infoFile = appDir </> "identity"
    
  doesFileExist cfgFile >>= \e -> unless e $ do
    dfile <- getDataFileName "default-config"
    BSL.readFile dfile >>= BSL.writeFile cfgFile
  
  cfg <- CFG.load [CFG.Required $ appDir </> "config"]
  
  -- initialize logging
  LOG.initLogging $ CFG.subconfig "logging" cfg

  -- read (maybe create) identity
  mNodeInfo <- doesFileExist infoFile >>= \e ->
    if e
    then JSON.decode <$> BSL.readFile infoFile
    else do
      nid <- getStdRandom randomId
      let result = NodeInfo nid ([] :: [TcpAddress])
      BSL.writeFile infoFile $ JSON.encode result
      return $ Just result

  -- start Freenet
  fn <- FN.initFn $ CFG.subconfig "freenet" cfg

  -- start our node
  case mNodeInfo of
    Nothing -> do
      logE $ "problem with " ++ infoFile
      shutdownFn fn
      
    Just nodeInfo -> do
      logI $ "node identity is " ++ (show $ nodeId nodeInfo)
      node <- mkNode nodeInfo fn
      readPeers node appDir
      nodeListen (CFG.subconfig "node.listen" cfg) node
      
      -- start HTTP Server
      startRestApi (CFG.subconfig "node.http" cfg) node
      
      -- start fproxy
      fpport <- CFG.lookup cfg "fproxy.port"
      case fpport of
        Nothing -> return ()
        Just p -> void $ forkIO $ Warp.run p $ FP.fproxy node

      -- wait for shutdown
      atomically $ readTVar shutdown >>= check

      shutdownFn fn
