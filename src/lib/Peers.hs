
{-# LANGUAGE OverloadedStrings #-}

module Peers (
  mkNodeInfo,

  PeerAddress(..), readPeers,
  Peer(..), mkPeer, peerFetchDone
  ) where

import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath ( (</>) )
import           System.IO.Error ( catchIOError )
import           System.Log.Logger

import           Freenet.Types
import           Message
import           Statistics
import           Types

logI :: String -> IO ()
logI = infoM "peers"

-- |
-- Extracts the persistable node information from a peer.
mkNodeInfo :: Peer a -> STM (NodeInfo a)
mkNodeInfo (Peer pid addrs _) = do
  as <- readTVar addrs
  return $ NodeInfo pid as

----------------------------------------------------------------
-- Peers
----------------------------------------------------------------

class (Show a, Eq a) => PeerAddress a where
  connectPeer :: Peer a -> (Either String (MessageIO a) -> IO ()) -> IO ()
  
-- |
-- This is the live version of @NodeInfo@, which can be constructed
-- from @NodeInfo@ and converted to @NodeInfo@ for storage and transfer.
data Peer a = Peer
            { peerId          :: ! Id
            , peerAddresses   :: ! (TVar [a]) -- ^ where this peer can be connected
            , peerSuccessEst  :: ! TEstimator -- ^ estimator for P(success) by location
            }

instance Eq (Peer a) where
  (==) p1 p2 = (peerId p1) == (peerId p2)

instance ToJSON a => ToStateJSON (Peer a) where
  toStateJSON (Peer pid adds ps) = do
    adds' <- readTVar adds
    ps'   <- toStateJSON ps
    
    return $ object
      [ "id"        .= pid
      , "addresses" .= adds'
      , "psuccess"  .= ps'
      ]
  
mkPeer :: PeerAddress a => NodeInfo a -> STM (Peer a)
mkPeer (NodeInfo nid addrs) = do
  as <- newTVar addrs
  ps <- mkTEstimator 128 0.01 0.5
  return $ Peer nid as ps

-- |
-- Update the peer statistics after a successful fetch from that peer.
peerFetchDone
  :: Peer a  -- ^ the peer who get it's status updated
  -> Key     -- ^ the key that was fetched
  -> Bool    -- ^ if the fetch was successful
  -> STM ()
peerFetchDone peer key suc = updateTEstimator (peerSuccessEst peer) (toLocation key) suc'
  where
    suc' = if suc then 1 else 0
    
readPeers
  :: (FromJSON a, PeerAddress a)
  => FilePath        -- ^ app data directory containing the peers file
  -> IO (Either String [NodeInfo a])
readPeers dataDir = do
  let
    kpFile = dataDir </> "peers"
    
  logI $ "reading known peers from " ++ kpFile
  catchIOError
    (fmap eitherDecode $ BSL.readFile kpFile)
    (\e -> return $ Left $ show e)
