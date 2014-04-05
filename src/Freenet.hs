
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, initFn, getChk, getSsk
  ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void, when )
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import qualified Data.Text as T
import System.FilePath ( (</>) )

import Freenet.Chk
import qualified Freenet.Companion as FC
import Freenet.Ssk
import qualified Freenet.Store as FS
import Freenet.Types

data Freenet a = FN
               { fnChkStore    :: FS.StoreFile ChkBlock
               , fnSskStore    :: FS.StoreFile SskBlock
               , fnCompanion   :: Maybe FC.Companion
               , fnIncomingChk :: TChan ChkBlock
               , fnIncomingSsk :: TChan SskBlock
               }

-- | initializes Freenet subsystem
initFn :: CFG.Config -> IO (Freenet a)
initFn cfg = do
  -- datastore
  dsdir       <- CFG.require cfg "datastore.directory"
  chkCount    <- CFG.require cfg "datastore.chk-count"
  chkStore    <- FS.mkStoreFile (undefined :: ChkBlock) (dsdir </> "store-chk") chkCount

  sskCount    <- CFG.require cfg "datastore.ssk-count"
  sskStore    <- FS.mkStoreFile (undefined :: SskBlock) (dsdir </> "store-ssk") sskCount
  
  chkIncoming <- newBroadcastTChanIO
  sskIncoming <- newBroadcastTChanIO
  
  let fn = FN chkStore sskStore Nothing chkIncoming sskIncoming

  -- companion
  let ccfg = CFG.subconfig "companion" cfg
  chost <- CFG.lookup ccfg "host" :: IO (Maybe String)
  case chost of
    Nothing -> return fn
    Just _  -> do
      comp <- FC.initCompanion ccfg (offerChk fn True) (offerSsk fn True)
      return $ fn { fnCompanion = Just comp }

offerSsk :: Freenet a -> Bool -> SskBlock -> IO ()
offerSsk fn toStore df = do
  when toStore $ FS.putData (fnSskStore fn) df
  atomically $ writeTChan (fnIncomingSsk fn) df

offerChk :: Freenet a -> Bool -> ChkBlock -> IO ()
offerChk fn toStore df = do
  when toStore $ FS.putData (fnChkStore fn) df
  atomically $ writeTChan (fnIncomingChk fn) df

waitKeyTimeout :: DataBlock f => TChan f -> Key -> IO (TMVar (Maybe f))
waitKeyTimeout chan loc = do
  bucket  <- newEmptyTMVarIO
  timeout <- registerDelay $ 30 * 1000 * 1000
  chan'   <- atomically $ dupTChan chan

  let
    doWait = orElse
      (readTChan chan' >>= \cf -> if dataBlockLocation cf == loc then putTMVar bucket (Just cf) else doWait)
      (readTVar timeout >>= \to -> if to then putTMVar bucket Nothing else retry)

  
  -- wait for data or timeout
  void $ forkIO $ atomically $ doWait
  
  return bucket

-- |
-- Tries to fetch CHK data either from the store or our
-- companion. Might block quite some time if the companion
-- is involved.
getChk :: Freenet a -> ChkRequest -> IO (Either T.Text ChkBlock)
getChk fn dr = do
  fromStore <- FS.getData (fnChkStore fn) (dataRequestLocation dr)
  
  case fromStore of
    Just blk -> return $ Right blk
    Nothing -> case fnCompanion fn of
        Nothing -> return $ Left "not in store, no companion"
        Just c  -> do
          bucket <- waitKeyTimeout (fnIncomingChk fn) (dataRequestLocation dr)
          FC.getChk c dr
          md <- atomically $ readTMVar bucket
  
          return $ case md of
            Nothing -> Left "timeout waiting for companion"
            Just d  -> Right d

getSsk :: Freenet a -> SskRequest -> IO (Either T.Text SskBlock)
getSsk fn dr = do
  fromStore <- FS.getData (fnSskStore fn) (dataRequestLocation dr)
  
  case fromStore of
    Just blk -> return $ Right blk
    Nothing -> case fnCompanion fn of
        Nothing -> return $ Left "not in store, no companion"
        Just c  -> do
          bucket <- waitKeyTimeout (fnIncomingSsk fn) (dataRequestLocation dr)
          FC.getSsk c dr
          md <- atomically $ readTMVar bucket
  
          return $ case md of
            Nothing -> Left "timeout waiting for companion"
            Just d  -> Right d

