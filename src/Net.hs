
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
import Control.Monad ( void )
import Data.Aeson
import Data.Binary
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Serialization.Binary
import Data.List ( nub )
import qualified Data.Vector as V
import System.IO.Error ( catchIOError )

import Logging
import Node
import Types

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
  
-- |
-- The type of addresses TCP/IP networking deals with
data TcpAddressInfo = AI [TcpAddress] deriving ( Eq, Show ) -- a list of (hostname, port)

instance Binary TcpAddressInfo where
  put (AI as) = put as
  get = AI <$> get

instance FromJSON TcpAddressInfo where
  parseJSON (Array as) = AI <$> (mapM parseJSON $ V.toList as)
  parseJSON _ = mzero

instance PeerAddress TcpAddressInfo where
  mergeAddress (AI l1) (AI l2) = AI $ nub $ l1 ++ l2
  
-- |
-- listens on the configured addresses and accepts incomping peer connections
nodeListen :: CFG.Config -> Node TcpAddressInfo -> IO ()
nodeListen cfg node = do
  host <- CFG.require cfg "host"
  port <- CFG.require cfg "port"

  let
    s = serverSettings port (fromString host)
    
  void $ forkIO $ runTCPServer s $ \ad -> do
    logI $ "incoming connection from " ++ (show $ appSockAddr ad)
    runPeerNode node (appSource ad $= conduitDecode, conduitEncode =$ appSink ad) Nothing

  infoM "net" $ "node listening on " ++ host ++ ":" ++ show port

tcpConnect :: ConnectFunction TcpAddressInfo
tcpConnect peer handler = do
  let (AI addrs) = peerAddress peer
  if null addrs
    then handler $ Left "no addresses"  -- TODO: deal with multiple adresses
    else let TcpAddress host port = head addrs in
      catchIOError
           (runTCPClient (clientSettings port $ BSC.pack host) $ \ad -> 
               handler $ Right (appSource ad $= conduitDecode, conduitEncode =$ appSink ad))
           (\e -> handler $ Left $ show e)
