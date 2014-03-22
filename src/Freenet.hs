
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, initFn, fetchUri
  ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void )
import qualified Data.ByteString as BS
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import qualified Freenet.Companion as FC
import qualified Freenet.Data as FD
import qualified Freenet.Metadata as MD
import qualified Freenet.Store as FS
import Freenet.Types
import qualified Freenet.URI as FU

newtype DataHandler = DataHandler (TMVar (Either T.Text FD.DataFound)) deriving ( Eq )

data Freenet = FN
               { fnStore     :: FS.FileStore
               , fnCompanion :: Maybe FC.Companion
               , fnRequests  :: TVar (Map.HashMap Key [DataHandler])
               }

-- | initializes Freenet subsystem
initFn :: CFG.Config -> IO Freenet
initFn cfg = do
  -- datastore
  dsdir <- CFG.require cfg "datastore"
  chkStore <- FS.mkFileStore 1024 dsdir
  reqs <- newTVarIO Map.empty
  
  let fn = FN chkStore Nothing reqs

  -- companion
  let ccfg = CFG.subconfig "companion" cfg
  chost <- CFG.lookup ccfg "host" :: IO (Maybe String)
  case chost of
    Nothing -> return fn
    Just _  -> do
      comp <- FC.initCompanion ccfg (offer fn)
      return $ fn { fnCompanion = Just comp }

offer :: Freenet -> FD.DataFound -> STM ()
offer fn df = do
  -- write to our store
  FS.putData (fnStore fn) df

  -- inform other registered handlers
  m <- readTVar (fnRequests fn)
  mapM_ (\(DataHandler bucket) -> putTMVar bucket (Right df)) $ Map.lookupDefault [] key m
  modifyTVar (fnRequests fn) (Map.delete key)
  where
    key = FD.dataFoundLocation df
    
-- FIXME: we cannot unregister handlers
addHandler :: Freenet -> Key -> DataHandler -> STM ()
addHandler fn key dh = modifyTVar (fnRequests fn) $ Map.insertWith (++) key [dh]

removeHandler :: Freenet -> Key -> DataHandler -> STM ()
removeHandler = error "removeHandler"

-- | creates a dataHandler which will set it's bucket to Nothing after a timeout
timeoutHandler :: Freenet -> Key -> IO DataHandler
timeoutHandler fn key = do
  timeout <- registerDelay (1000 * 1000 * 10)
  
  dh@(DataHandler bucket) <- atomically $ do
    dfc <- newEmptyTMVar
    let dh = DataHandler dfc
    addHandler fn key dh
    return dh
  
  void $ forkIO $ atomically $ do
    empty <- isEmptyTMVar bucket

    if empty
      then readTVar timeout >>= \to -> if to
                                       then putTMVar bucket (Left "timeout") >> removeHandler fn key dh
                                       else retry
      else return ()
    
  return dh
  
handleRequest :: Freenet -> DataRequest -> IO ()
handleRequest fn dr = do
  fromStore <- FS.getData (fnStore fn) dr
  
  case fromStore of
    Just df -> atomically $ offer fn df
    Nothing -> case fnCompanion fn of
      Nothing -> return ()
      Just c  -> FC.getData c dr
      
fetchUri :: Freenet -> FU.URI -> IO (Either T.Text BS.ByteString)
fetchUri fn uri = do
  let dr = FU.toDataRequest uri
  (DataHandler bucket) <- timeoutHandler fn $ FU.uriLocation uri
  handleRequest fn dr
  
  df <- atomically $ takeTMVar bucket
  
  case df of
    Left e  -> return $ Left e
    Right d -> do
      case FD.decryptDataFound (FU.chkKey uri) d of
        Left decError -> return $ Left decError
        Right plain   -> case MD.parseMetadata plain of
          Left e   -> return $ Left e
          Right md -> return (Right plain)
  
