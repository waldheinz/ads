
{-# LANGUAGE OverloadedStrings #-}

-- Lower - level Networking
module Net (
  nodeListen
  ) where

import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import Data.String ( fromString )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void )
import Data.Conduit.Network

import Node as N
import Peers as P

nodeListen :: CFG.Config -> P.Peers -> IO ()
nodeListen cfg p = do
  host <- CFG.require cfg "host"
  port <- CFG.require cfg "port"

  let
    s = serverSettings port ( fromString host)
  void $ forkIO $ runTCPServer s $ \ad -> do
      print $ ("client connected", appSockAddr ad)
      N.runNode (appSource ad) (appSink ad) Nothing $ \n -> do
        print n
        atomically $ P.addPeer p n
