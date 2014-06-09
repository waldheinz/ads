
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( unless, void )
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import Network ( withSocketsDo )
import Network.Wai.Handler.Warp as Warp
import System.Directory ( createDirectoryIfMissing, doesFileExist, getAppUserDataDirectory )
import System.Environment ( getArgs )
import System.FilePath ( (</>) )
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import System.Random ( getStdRandom )

import Paths_ads

import Freenet as FN
import Freenet.Chk
import Freenet.Fproxy as FP
import Freenet.Rijndael as RD
import Freenet.Ssk
import Logging as LOG
import Net
import Node
import RestApi
import Store
import Types

logI :: String -> IO ()
logI = infoM "main"

logW :: String -> IO ()
logW = warningM "main"

logE :: String -> IO ()
logE = errorM "main"

sigHandler :: TVar Bool -> IO ()
sigHandler s = do
  infoM "main" "shutdown on sigint/sigterm"
  atomically $ writeTVar s True
  
  
mkFileStore :: CFG.Config -> IO (StoreFile ChkBlock, StoreFile SskBlock)
mkFileStore cfg = do
    -- datastore
  dsdir       <- CFG.require cfg "datastore.directory"
  createDirectoryIfMissing True dsdir
  
  chkCount    <- CFG.require cfg "datastore.chk-count"
  chkStore    <- mkStoreFile (undefined :: ChkBlock) (dsdir </> "store-chk") chkCount

  sskCount    <- CFG.require cfg "datastore.ssk-count"
  sskStore    <- mkStoreFile (undefined :: SskBlock) (dsdir </> "store-ssk") sskCount

  return (chkStore, sskStore)
  
main :: IO ()
main = withSocketsDo $ do
  infoM "main" "Starting up..."
  
  RD.initRijndael

  -- find and maybe create home directory
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

  let fnCfg = CFG.subconfig "freenet" cfg
  
  -- start Freenet
  fn <- FN.initFn fnCfg
  (chks, ssks) <- mkFileStore fnCfg

  let
    shutdownStores :: IO ()
    shutdownStores  = do
      shutdownStore chks
      shutdownStore ssks

  -- start our node
  case mNodeInfo of
    Nothing -> do
      logE $ "problem with " ++ infoFile
      shutdownStores
      
    Just nodeInfo -> do
      logI $ "node identity is " ++ (show $ nodeId nodeInfo)
      node <- mkNode nodeInfo fn chks ssks
      
      readPeers appDir >>= \ps ->  case ps of
        Left  e     -> logW ("error parsing peers file: " ++ e)
        Right peers -> do
          logI ("got " ++ show (length peers) ++ " peers")
          atomically $ mapM_ (mergeNodeInfo node) peers

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
      shutdownStores
