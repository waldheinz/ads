
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, initFn, requestData,

  -- * Talking to other nodes
  handleMessage
  ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void, when )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import qualified Data.Text as T
import System.FilePath ( (</>) )
import System.Log.Logger

import Freenet.Chk
import qualified Freenet.Companion as FC
import Freenet.Keys
import Freenet.Messages
import Freenet.Ssk
import qualified Freenet.Store as FS
import Freenet.Types
import Freenet.URI

data Freenet = FN
               { fnChkStore    :: FS.StoreFile ChkFound
               , fnCompanion   :: Maybe FC.Companion
               , fnIncomingChk :: TChan ChkFound
               , fnIncomingSsk :: TChan SskFound
               }

logI :: String -> IO ()
logI m = infoM "freenet" m

-- | initializes Freenet subsystem
initFn :: CFG.Config -> IO Freenet
initFn cfg = do
  -- datastore
  dsdir       <- CFG.require cfg "datastore.directory"
  chkCount    <- CFG.require cfg "datastore.chk-count"
  chkStore    <- FS.mkStoreFile (undefined :: ChkFound) (dsdir </> "store-chk") chkCount
  
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

handleMessage :: Freenet -> Message -> IO ()
handleMessage fn msg = print ("freenet handleMessage", msg)

offerSsk :: Freenet -> Bool -> SskFound -> STM ()
offerSsk fn _ df = do
  -- write to our store
--  when toStore $ FS.putData (fnSskStore fn) df
  
  -- broadcast arrival
  writeTChan (fnIncomingSsk fn) df

offerChk :: Freenet -> Bool -> ChkFound -> STM ()
offerChk fn toStore df = do
  -- write to our store
  when toStore $ FS.putData (fnChkStore fn) df
  
  -- broadcast arrival
  writeTChan (fnIncomingChk fn) df

handleDataRequest :: Freenet -> DataRequest -> IO ()
handleDataRequest fn dr = do
  fromStore <- case dr of
    (ChkRequest { }) -> FS.getData (fnChkStore fn) dr
    _ -> return Nothing
  
  case fromStore of
    Just df -> atomically $ offerChk fn False df
    Nothing -> case fnCompanion fn of
      Nothing -> return ()
      Just c  -> FC.get c dr

waitKeyTimeout :: DataFound f => TChan f -> Key -> IO (TMVar (Maybe f))
waitKeyTimeout chan loc = do
  bucket  <- newEmptyTMVarIO
  timeout <- registerDelay $ 10 * 1000 * 1000
  chan'    <- atomically $ dupTChan chan
  
  -- wait for data or timeout
  void $ forkIO $ atomically $ orElse
    (readTChan chan' >>= \cf -> if dataFoundLocation cf == loc then putTMVar bucket (Just cf) else retry)
    (readTVar timeout >>= \to -> if to then putTMVar bucket Nothing else retry)

  return bucket

requestData :: Freenet -> URI -> IO (Either T.Text BSL.ByteString)
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
