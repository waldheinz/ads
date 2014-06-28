
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Applicative ( (<$>) )
import           Control.Concurrent ( forkIO )
import           Control.Concurrent.STM
import           Control.Monad ( unless, void )
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import           Network ( withSocketsDo )
import           Network.Wai.Handler.Warp as Warp
import           System.Directory ( createDirectoryIfMissing, doesFileExist, getAppUserDataDirectory )
import           System.Environment ( getArgs )
import           System.FilePath ( (</>) )
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import           System.Random ( getStdRandom )

import           Paths_ads

import           Freenet
import           Freenet.Chk
import           Freenet.Fproxy as FP
import           Freenet.Rijndael as RD
import           Freenet.Ssk
import           Freenet.Store
import           Logging as LOG
import           Net
import           Node
import           Peers
import           RestApi
import           Types

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
  
mkFn :: Node a -> CFG.Config -> IO (Freenet a)
mkFn node cfg = do
  dsdir       <- CFG.require cfg "datastore.directory"
  createDirectoryIfMissing True dsdir
  
  chkCount    <- CFG.require cfg "datastore.chk-count"
  chkStore    <- mkStoreFile (dsdir </> "store-chk") chkCount

  sskCount    <- CFG.require cfg "datastore.ssk-count"
  sskStore    <- mkStoreFile (dsdir </> "store-ssk") sskCount
  
  atomically $ mkFreenet node Nothing chkStore sskStore

-- |
-- Assembles the HTTP server and starts it up.
startHttpServer :: (PeerAddress a, JSON.ToJSON a) => CFG.Config -> Node a -> IO ()
startHttpServer cfg node = do
  -- start HTTP Server
  startRestApi (CFG.subconfig "node.http" cfg) node
      
  -- start fproxy
  fpport <- CFG.lookup cfg "fproxy.port" :: IO (Maybe Int)
  case fpport of
    Nothing -> return ()
    Just p -> error "need resolver for fproxy" -- void $ forkIO $ Warp.run p $ FP.fproxy node
  
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

  LOG.initLogging

  -- load (and maybe create) configuration file
  let
    cfgFile  = appDir </> "config"
    infoFile = appDir </> "identity"
    
  doesFileExist cfgFile >>= \e -> unless e $ do
    dfile <- getDataFileName "default-config"
    BSL.readFile dfile >>= BSL.writeFile cfgFile
  
  cfg <- CFG.load [CFG.Required $ appDir </> "config"]

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
  
  -- start our node
  case mNodeInfo of
    Nothing -> do
      logE $ "problem with " ++ infoFile
      
    Just nodeInfo -> do
      logI $ "node identity is " ++ (show $ nodeId nodeInfo)
      node <- mkNode nodeInfo
      fn   <- mkFn node fnCfg 

      startHttpServer cfg node
      
      readPeers appDir >>= \ps ->  case ps of
        Left  e     -> logW ("error parsing peers file: " ++ e)
        Right peers -> do
          logI ("got " ++ show (length peers) ++ " peers")
          atomically $ mapM_ (mergeNodeInfo node) peers

      nodeListen (CFG.subconfig "node.listen" cfg) node
      
      -- wait for shutdown
      atomically $ readTVar shutdown >>= check
      shutdownFreenet fn
