
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, initFn, fetchUri
  ) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import Data.Digest.Pure.SHA
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Freenet.Base64
import qualified Freenet.Companion as FC
import qualified Freenet.Keys as FK
import qualified Freenet.Metadata as MD
import qualified Freenet.Store as FS
import Freenet.Types
import qualified Freenet.URI as FU

data Freenet = FN
               { fnChkStore  :: FS.FileStore FK.CHK'
               , fnCompanion :: Maybe FC.Companion
               , fnRequests  :: TVar (Map.HashMap Key [DataHandler])
               }

-- | initializes Freenet subsystem
initFn :: CFG.Config -> IO Freenet
initFn cfg = do
  -- datastore
  dsdir <- CFG.require cfg "datastore"
  chkStore <- FS.mkFileStore undefined 1024 (dsdir ++ "/chkstore" )
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

offer :: Freenet -> DataHandler
offer fn df@(ChkFound key _ _) = do
  m <- readTVar (fnRequests fn)
  mapM_ (\h -> h df) $ Map.lookupDefault [] key m
  modifyTVar (fnRequests fn) (Map.delete key)

-- FIXME: we cannot unregister handlers
addHandler :: Freenet -> Key -> DataHandler -> STM ()
addHandler fn key dh = modifyTVar (fnRequests fn) $ Map.insertWith (++) key [dh]

waitDataFound :: Freenet -> Key -> IO (Either T.Text DataFound)
waitDataFound fn key = do
  timeout <- registerDelay (1000 * 1000 * 10)
  
  df <- atomically $ do
    dfc <- newEmptyTMVar
    addHandler fn key (putTMVar dfc)
    return dfc

  atomically $ do
    x <- tryReadTMVar df
    case x of
      Just d  -> return $ Right d
      Nothing -> readTVar timeout >>= \to -> if to
                                             then return $ Left "timeout"
                                             else retry
  
handleRequest :: Freenet -> DataRequest -> IO ()
handleRequest fn dr = do
  case fnCompanion fn of
    Nothing -> return ()
    Just c  -> FC.getData c dr

fetchUri :: Freenet -> FU.URI -> IO (Either T.Text BS.ByteString)
fetchUri fn uri = do
  let dr = FU.toDataRequest uri
  handleRequest fn dr
  df <- waitDataFound fn (FU.uriLocation uri)
  
  case df of
    Left e  -> return $ Left e
    Right d@(ChkFound k hdr da) -> do
      print ("key", k)
      print ("headers", (toBase64' . unChkHeader) hdr)
      print ("hash", showDigest $ sha256 $ BSL.fromStrict da)
      
      case FK.decryptDataFound (FU.chkKey uri) d of
        Left decError -> return $ Left decError
        Right plain   -> case MD.parseMetadata plain of
          Left e   -> return $ Left e
          Right md -> print md >> return (Right plain)
  
