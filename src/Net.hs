
{-# LANGUAGE OverloadedStrings #-}

-- |
-- TCP/IP Networking
module Net (
  TcpAddress(..),
  nodeListen, tcpConnect
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent.STM
import Control.Monad ( mzero )
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import Data.String ( fromString )
import Control.Concurrent ( forkIO )
import Control.Monad ( void )
import Data.Aeson
import Data.Binary
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Serialization.Binary
import System.IO.Error ( catchIOError )

import Logging
import Node
import Peers

logD :: String -> IO ()
logD = debugM "net"

logI :: String -> IO ()
logI m = infoM "net" m

data TcpAddress = TcpAddress String Int deriving ( Eq, Show )

instance Binary TcpAddress where
  put (TcpAddress h p) = put h >> put p
  get = TcpAddress <$> get <*> get

instance FromJSON TcpAddress where
  parseJSON (Object v) = TcpAddress <$>
                         v .: "host" <*>
                         v .: "port"
  parseJSON _ = mzero

instance ToJSON TcpAddress where
  toJSON (TcpAddress h p) = object
                            [ "host" .= h
                            , "port" .= p
                            ]
  
instance PeerAddress TcpAddress where
  
-- |
-- listens on the configured addresses and accepts incomping peer connections
nodeListen :: CFG.Config -> Node TcpAddress -> IO ()
nodeListen cfg node = do
  host <- CFG.require cfg "host"
  port <- CFG.require cfg "port"

  let
    s = serverSettings port (fromString host)
    
  void $ forkIO $ runTCPServer s $ \ad -> do
    logI $ "incoming connection from " ++ (show $ appSockAddr ad)
    runPeerNode node (appSource ad $= conduitDecode, conduitEncode =$ appSink ad) Nothing

  infoM "net" $ "node listening on " ++ host ++ ":" ++ show port

-- |
-- Connects to a @Peer@ using TCP sockets.
tcpConnect :: ConnectFunction TcpAddress
tcpConnect peer handler = do
  addrs <- atomically . readTVar $ peerAddresses peer
  
  let
    tryConnect [] = handler $ Left "no addresses left"
    tryConnect (x@(TcpAddress host port) : xs) = do
      catchIOError
        (do
            logI $ "connecting to " ++ show (peerId peer) ++ " @ " ++ show x
            runTCPClient (clientSettings port $ BSC.pack host) $ \ad -> do
              logI $ "connected to " ++ show x
              handler $ Right (appSource ad $= conduitDecode, conduitEncode =$ appSink ad))
        (\e -> do
            logD $ "error connecting to " ++ show (peerId peer) ++ " @ " ++ show x ++ ": " ++ show e
            tryConnect xs)
        
  tryConnect addrs
