
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, initFn
  ) where

import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG

import qualified Freenet.Keys as FK
import qualified Freenet.Store as FS

data Freenet = FN
               { fnChkStore :: FS.FileStore FK.CHK
               }

-- | initialize Freenet subsystem
initFn :: CFG.Config -> IO Freenet
initFn cfg = do
  dsdir <- CFG.require cfg "datastore"

  chkStore <- FS.mkFileStore undefined 1024 (dsdir ++ "/chkstore" )
  
  return $! FN chkStore
  
