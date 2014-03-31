
{-# LANGUAGE OverloadedStrings #-}

module Types (
  NodeId, mkNodeId', NodeInfo(..),

  Peer(..), mkPeer
  ) where

import Control.Applicative ( pure, (<$>), (<*>) )
import Control.Monad ( mzero )
import Data.Aeson
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits ( shiftL )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as HEX
import qualified Data.ByteString.Char8 as BSC
import Data.Ratio ( (%) )
import Data.Text.Encoding ( encodeUtf8 )

import qualified NextBestOnce as NBO

----------------------------------------------------------------------
-- Node IDs
----------------------------------------------------------------------

newtype NodeId = NodeId { unNodeId :: BS.ByteString } deriving ( Eq )

instance Binary NodeId where
  put = putByteString . unNodeId
  get = NodeId <$> getByteString 32
  
instance Show NodeId where
  show nid = BSC.unpack $ HEX.encode (unNodeId nid)

instance FromJSON NodeId where
  parseJSON (String s) = pure $ NodeId $ fst (HEX.decode $ encodeUtf8 s)
  parseJSON _ = mzero

nodeIdToInteger :: NodeId -> Integer
nodeIdToInteger (NodeId bs) = BS.foldl' (\i bb -> (i `shiftL` 8) + fromIntegral bb) 0 bs

maxNodeId :: Integer
maxNodeId = 255 ^ (32 :: Integer)

instance NBO.Location NodeId where
  toDouble nid = fromRational $ (nodeIdToInteger nid) % maxNodeId

mkNodeId' :: BS.ByteString -> NodeId
mkNodeId' bs
  | BS.length bs /= 32 = error "mkNodeId': expected 32 bytes"
  | otherwise = NodeId bs

----------------------------------------------------------------------
-- Node Info
----------------------------------------------------------------------
  
data NodeInfo = NodeInfo
                { nodeId :: NodeId -- ^ the globally unique node ID of 256 bits
                } deriving ( Eq, Show )

instance Binary NodeInfo where
  put ni = put $ nodeId ni
  get = NodeInfo <$> get
  
instance FromJSON NodeInfo where
  parseJSON (Object v) = NodeInfo <$>
                         v .: "id"
  parseJSON _ = mzero
  

----------------------------------------------------------------
-- Peers / Peer Nodes
----------------------------------------------------------------


data (Show a) => Peer a = Peer
            { peerNodeInfo :: NodeInfo        -- ^ the static node info of this peer
            , peerAddress  :: a               -- ^ where this peer might be connected
            } deriving ( Show )

instance (Show a) => Eq (Peer a) where
  (==) p1 p2 = (peerNodeInfo p1) == (peerNodeInfo p2)

instance (Show a, FromJSON a) => FromJSON (Peer a) where
  parseJSON (Object v) = Peer <$>
                         v .: "node" <*>
                         v .: "address"
  parseJSON _ = mzero

instance (Binary a, Show a) => Binary (Peer a) where
  put (Peer ni addr) = put ni >> put addr
  get = Peer <$> get <*> get

mkPeer :: (Show a) => NodeInfo -> a -> Peer a
mkPeer ni addr = Peer ni addr

