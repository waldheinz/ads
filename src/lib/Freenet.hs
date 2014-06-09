
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, initFn,
  getChk, getSsk,
  offerChk, offerSsk
  ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void )
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import qualified Data.Text as T

import Freenet.Chk
import qualified Freenet.Companion as FC
import Freenet.Ssk
import Freenet.Types

data Freenet a = FN
               { fnCompanion   :: Maybe FC.Companion
               , fnIncomingChk :: TChan ChkBlock
               , fnIncomingSsk :: TChan SskBlock
               }

-- | initializes Freenet subsystem
initFn :: CFG.Config -> IO (Freenet a)
initFn cfg = do
  
  chkIncoming <- newBroadcastTChanIO
  sskIncoming <- newBroadcastTChanIO
  
  let fn = FN Nothing chkIncoming sskIncoming

  -- companion
  let ccfg = CFG.subconfig "companion" cfg
  chost <- CFG.lookup ccfg "host" :: IO (Maybe String)
  case chost of
    Nothing -> return fn
    Just _  -> do
      comp <- FC.initCompanion ccfg (offerChk fn) (offerSsk fn)
      return $ fn { fnCompanion = Just comp }
  
offerSsk :: Freenet a -> SskBlock -> IO ()
offerSsk fn df = do
  atomically $ writeTChan (fnIncomingSsk fn) df

offerChk :: Freenet a -> ChkBlock -> IO ()
offerChk fn df = do
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
getChk fn dr = case fnCompanion fn of
  Nothing -> return $ Left "not in store, no companion"
  Just c  -> do
    bucket <- waitKeyTimeout (fnIncomingChk fn) (dataRequestLocation dr)
    FC.getChk c dr
    md <- atomically $ readTMVar bucket
  
    return $ case md of
      Nothing -> Left "timeout waiting for companion"
      Just d  -> Right d

getSsk :: Freenet a -> SskRequest -> IO (Either T.Text SskBlock)
getSsk fn dr = case fnCompanion fn of
  Nothing -> return $ Left "not in store, no companion"
  Just c  -> do
    bucket <- waitKeyTimeout (fnIncomingSsk fn) (dataRequestLocation dr)
    FC.getSsk c dr
    md <- atomically $ readTMVar bucket
    
    return $ case md of
      Nothing -> Left "timeout waiting for companion"
      Just d  -> Right d

