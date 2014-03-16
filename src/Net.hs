
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

import Logging
import qualified Message as MSG
import Node as N
import Peers as P

nodeListen :: CFG.Config -> NodeInfo -> P.Peers -> IO ()
nodeListen cfg ni p = do
  host <- CFG.require cfg "host"
  port <- CFG.require cfg "port"

  let
    s = serverSettings port (fromString host)
  void $ forkIO $ runTCPServer s $ \ad -> do
    infoM "net" $ "incoming connection from " ++ (show $ appSockAddr ad)
    N.runNode (appSource ad) (appSink ad) Nothing $ \n -> do
      atomically $ P.addPeer p n >> N.enqMessage n (MSG.Hello ni)
      infoM "net" $ "added peer " ++ show n

  infoM "net" $ "Node listening on " ++ host ++ ":" ++ show port
