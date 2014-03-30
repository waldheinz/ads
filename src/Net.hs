
{-# LANGUAGE OverloadedStrings #-}

-- |
-- TCP/IP Networking
module Net (
  TcpAddressInfo(..),
  nodeListen, tcpConnect
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( mzero )
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import Data.String ( fromString )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void )
import Data.Aeson
import Data.Binary
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Serialization.Binary
import qualified Data.Vector as V

import Logging
import qualified Message as MSG
--import Node as N
import Peers as P
import Types

logI :: String -> IO ()
logI m = infoM "net" m

data TcpAddress = TcpAddress String Int deriving ( Show )

instance Binary TcpAddress where
  put (TcpAddress h p) = put h >> put p
  get = TcpAddress <$> get <*> get

instance FromJSON TcpAddress where
  parseJSON (Object v) = TcpAddress <$>
                         v .: "host" <*>
                         v .: "port"
  parseJSON _ = mzero
  
-- |
-- The type of addresses TCP/IP networking deals with
data TcpAddressInfo = AI [TcpAddress] deriving ( Show ) -- a list of (hostname, port)

instance Binary TcpAddressInfo where
  put (AI as) = put as
  get = AI <$> get

instance FromJSON TcpAddressInfo where
  parseJSON (Array as) = AI <$> (mapM parseJSON $ V.toList as)
  parseJSON _ = mzero
  
-- |
-- listens on the configured addresses and accepts incomping peer connections
nodeListen :: CFG.Config -> Peer TcpAddressInfo -> P.Peers TcpAddressInfo -> IO ()
nodeListen cfg ni p = do
  host <- CFG.require cfg "host"
  port <- CFG.require cfg "port"

  let
    s = serverSettings port (fromString host)
  void $ forkIO $ runTCPServer s $ \ad -> do
    infoM "net" $ "incoming connection from " ++ (show $ appSockAddr ad)
    P.runPeerNode (appSource ad $= conduitDecode, conduitEncode =$ appSink ad) Nothing $ \n -> do
      atomically $ P.addPeer p n >> P.enqMessage n (MSG.Hello ni)
      logI $ "added " ++ show n

  infoM "net" $ "node listening on " ++ host ++ ":" ++ show port

tcpConnect :: ConnectFunction TcpAddressInfo
tcpConnect peer handler = do
  let (AI addrs) = peerAddress peer
  if null addrs
    then handler $ Left "no addresses"
    else let TcpAddress host port = head addrs in do -- TODO: deal with multiple adresses
      runTCPClient (clientSettings port $ BSC.pack host) $ \ad -> do
        handler $ Right (appSource ad $= conduitDecode, conduitEncode =$ appSink ad)
