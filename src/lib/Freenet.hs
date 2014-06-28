
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, mkFreenet, shutdownFreenet
  ) where

import           Control.Concurrent ( forkIO )
import           Control.Concurrent.STM
import           Control.Monad ( void )
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import qualified Data.Text as T

import           Freenet.Chk
import           Freenet.Companion
import           Freenet.Ssk
import Freenet.Store
import           Freenet.Types
import           Node

data Freenet a = Freenet
                 { fnNode      :: Node a
                 , fnCompanion :: Maybe Companion
                 , fnChkStore  :: StoreFile ChkBlock
                 , fnSskStore  :: StoreFile SskBlock
                 }

mkFreenet
  :: Node a
  -> Maybe Companion
  -> StoreFile ChkBlock
  -> StoreFile SskBlock
  -> STM (Freenet a)
mkFreenet node comp chk ssk = do
  return $ Freenet node comp chk ssk

shutdownFreenet :: Freenet a -> IO ()
shutdownFreenet fn = do
  shutdownStore $ fnChkStore fn
  shutdownStore $ fnSskStore fn
  
{-
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
  -}

