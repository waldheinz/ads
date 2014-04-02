
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, initFn, getChk,
  ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void, when )
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import qualified Data.Text as T
import System.FilePath ( (</>) )
import System.Log.Logger

import Freenet.Chk
import qualified Freenet.Companion as FC
import Freenet.Ssk
import qualified Freenet.Store as FS
import Freenet.Types

data Freenet a = FN
               { fnChkStore    :: FS.StoreFile ChkBlock
               , fnCompanion   :: Maybe FC.Companion
               , fnIncomingChk :: TChan ChkBlock
               , fnIncomingSsk :: TChan SskFound
               }

logI :: String -> IO ()
logI m = infoM "freenet" m

-- | initializes Freenet subsystem
initFn :: CFG.Config -> IO (Freenet a)
initFn cfg = do
  -- datastore
  dsdir       <- CFG.require cfg "datastore.directory"
  chkCount    <- CFG.require cfg "datastore.chk-count"
  chkStore    <- FS.mkStoreFile (undefined :: ChkBlock) (dsdir </> "store-chk") chkCount
  
  chkIncoming <- newBroadcastTChanIO
  sskIncoming <- newBroadcastTChanIO
  
  let fn = FN chkStore Nothing chkIncoming sskIncoming

  -- companion
  let ccfg = CFG.subconfig "companion" cfg
  chost <- CFG.lookup ccfg "host" :: IO (Maybe String)
  case chost of
    Nothing -> return fn
    Just _  -> do
      comp <- FC.initCompanion ccfg (offerChk fn True) (offerSsk fn True)
      return $ fn { fnCompanion = Just comp }

offerSsk :: Freenet a -> Bool -> SskFound -> STM ()
offerSsk fn _ df = do
  -- write to our store
--  when toStore $ FS.putData (fnSskStore fn) df
  
  -- broadcast arrival
  writeTChan (fnIncomingSsk fn) df

offerChk :: Freenet a -> Bool -> ChkBlock -> STM ()
offerChk fn toStore df = do
  -- write to our store
  when toStore $ FS.putData (fnChkStore fn) df
  
  -- broadcast arrival
  writeTChan (fnIncomingChk fn) df

waitKeyTimeout :: DataBlock f => TChan f -> Key -> IO (TMVar (Maybe f))
waitKeyTimeout chan loc = do
  bucket  <- newEmptyTMVarIO
  timeout <- registerDelay $ 10 * 1000 * 1000
  chan'    <- atomically $ dupTChan chan
  
  -- wait for data or timeout
  void $ forkIO $ atomically $ orElse
    (readTChan chan' >>= \cf -> if dataBlockLocation cf == loc then putTMVar bucket (Just cf) else retry)
    (readTVar timeout >>= \to -> if to then putTMVar bucket Nothing else retry)

  return bucket
  
{-
requestData :: Freenet a -> URI -> IO (Either T.Text BSL.ByteString)
requestData fn uri = logI ("data request for " ++ show uri) >> let dr = toDataRequest uri in case dr of
  ChkRequest {} -> do
    let 
      l = dataRequestLocation dr
      chan = fnIncomingChk fn
      
    bucket <- waitKeyTimeout (chan) l
    handleDataRequest fn dr
    md <- atomically $ readTMVar bucket
  
    return $ case md of
      Nothing -> Left "requestData: timeout"
      Just d  -> decryptDataFound d (uriCryptoKey uri) (uriCryptoAlg uri)
      
  SskRequest {} -> do
    let 
      l = dataRequestLocation dr
      chan = fnIncomingSsk fn
      
    bucket <- waitKeyTimeout (chan) l
    handleDataRequest fn dr
    md <- atomically $ readTMVar bucket
    
    return $ case md of
      Nothing -> Left "requestData: timeout"
      Just d  -> decryptDataFound d (uriCryptoKey uri) (uriCryptoAlg uri)
-}

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
