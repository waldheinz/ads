
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Maintain connections to other peers.
module Peers (
  PeerIO, ConnectFunction,
  Peers, initPeers, addPeer
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( forever, void )
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit
import Data.List ( (\\) )
import System.FilePath ( (</>) )
import System.Log.Logger

import Message
import Node as N
import Types

type PeerIO a = (Source IO (Message a), Sink (Message a) IO ())
type ConnectFunction a = Peer a -> IO (Either String (PeerIO a))

----------------------------------------------------------------
-- the peers list
----------------------------------------------------------------
  
data Peers a = Peers
             { peersConnected  :: TVar [Node a] -- ^ peers we're currently connected to
             , peersConnecting :: TVar [Peer a] -- ^ peers we're currently trying to connect to
             , peersKnown      :: TVar [Peer a] -- ^ all peers we know about, includes above
             }

logI :: String -> IO ()
logI m = infoM "peers" m

logW :: String -> IO ()
logW m = warningM "peers" m

initPeers :: (Show n, FromJSON n) => Peer n -> ConnectFunction n -> FilePath -> IO (Peers n)
initPeers identity connect dataDir = do
  let
    kpFile = dataDir </> "peers"
    
  logI $ "reading known peers from " ++ kpFile
  kpbs <- BSL.readFile kpFile
  
  pk <- case eitherDecode kpbs of
    Left  e     -> logW ("error parsing peers file: " ++ e) >> return []
    Right peers -> logI ("got " ++ (show $ length peers) ++ " peers") >> return peers

  ps <- atomically $ Peers <$> newTVar [] <*> newTVar [] <*> newTVar pk
  void $ forkIO $ maintainConnections identity connect ps
  return ps

addPeer :: Peers a -> Node a -> STM ()
addPeer p n = modifyTVar' (peersConnected p) ((:) n)

maintainConnections :: (Show a) => Peer a -> ConnectFunction a -> Peers a -> IO ()
maintainConnections identity connect peers = forever $ do
  -- we simply try to maintain a connection to all known peers for now

  shouldConnect <- atomically $ do
    known <- readTVar $ peersKnown peers
    cting <- readTVar $ peersConnecting peers
    connected <- readTVar $ peersConnected peers
  
    let
      result = known \\ cting
      result' = result \\ (map N.nodePeer connected)

    if null result'
      then retry
      else do
        let result'' = head result'
        modifyTVar (peersConnecting peers) ((:) result'')
        return result''
        
  logI $ "connecting to " ++ show shouldConnect
  cresult <- connect shouldConnect
  case cresult of
    Left e -> do
      logW $ "error connecting: " ++ e ++ " on " ++ show shouldConnect
      atomically $ modifyTVar (peersConnecting peers) (filter ((==) shouldConnect))
    Right (src, sink) -> do
      logI "connected!"
      runNode src sink (Just identity) $ \node -> do
        print node
  
