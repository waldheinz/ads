
{-# LANGUAGE OverloadedStrings #-}

module Types (
  NodeId, mkNodeId', randomNodeId, keyToNodeId,
  nodeIdToDouble,
  NodeInfo(..),
  
  -- * state aware serialization
  ToStateJSON(..),
  
  UriFetch(..)
  ) where

import Control.Applicative ( pure, (<$>), (<*>) )
import Control.Concurrent.STM
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
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import System.Random ( RandomGen, random )

import qualified Freenet.URI as FN
import qualified Freenet.Types as FN
import qualified NextBestOnce as NBO

----------------------------------------------------------------------
-- STM aware serialization
----------------------------------------------------------------------

class ToStateJSON a where
  toStateJSON :: a -> STM Value

instance ToStateJSON a => ToStateJSON [a] where
  toStateJSON xs = toJSON <$> mapM toStateJSON xs

instance ToStateJSON a => ToStateJSON (TVar a) where
  toStateJSON v = readTVar v >>= toStateJSON

----------------------------------------------------------------------
-- Node IDs
----------------------------------------------------------------------

newtype NodeId = NodeId { unNodeId :: BS.ByteString } deriving ( Eq )

instance Binary NodeId where
  put = putByteString . unNodeId
  get = NodeId <$> getByteString 32
  
instance Show NodeId where
  show nid = BSC.unpack (HEX.encode $ unNodeId nid) ++ " (" ++ show (nodeIdToDouble nid) ++ ")"

instance FromJSON NodeId where
  parseJSON (String s) = pure $ NodeId $ fst (HEX.decode $ encodeUtf8 s)
  parseJSON _ = mzero

instance ToJSON NodeId where
  toJSON (NodeId bs) = toJSON $ decodeUtf8 $ HEX.encode bs

nodeIdToInteger :: NodeId -> Integer
nodeIdToInteger (NodeId bs) = BS.foldl' (\i bb -> (i `shiftL` 8) + fromIntegral bb) 0 bs

maxNodeId :: Integer
maxNodeId = (256 ^ (32 :: Integer)) - 1

nodeIdToDouble :: NodeId -> Double
nodeIdToDouble nid = fromRational $ (nodeIdToInteger nid) % maxNodeId

instance NBO.Location NodeId where
  toDouble l = nodeIdToDouble l

mkNodeId' :: BS.ByteString -> NodeId
mkNodeId' bs
  | BS.length bs /= 32 = error "mkNodeId': expected 32 bytes"
  | otherwise = NodeId bs

-- |
-- Turn a Freenet @Key@ into a @NodeId@ by repacking the 32 bytes.
keyToNodeId :: FN.Key -> NodeId
keyToNodeId key = mkNodeId' $ FN.unKey key

randomNodeId :: RandomGen g => g -> (NodeId, g)
randomNodeId g = let (bs, Just g') = BS.unfoldrN 32 (Just . random) g in (mkNodeId' bs, g')

----------------------------------------------------------------------
-- Node Info
----------------------------------------------------------------------

-- |
-- The node information which can be exchanged between peers.
data NodeInfo a = NodeInfo
                  { nodeId        :: NodeId -- ^ the globally unique node ID of 256 bits
                  , nodeAddresses :: [a]
                  } deriving ( Eq, Show )

instance Binary a => Binary (NodeInfo a) where
  put ni = put (nodeId ni) >> put (nodeAddresses ni)
  get = NodeInfo <$> get <*> get
  
instance FromJSON a => FromJSON (NodeInfo a) where
  parseJSON (Object v) = NodeInfo
                         <$> v .: "id"
                         <*> v .: "addresses"
  parseJSON _ = mzero

instance ToJSON a => ToJSON (NodeInfo a) where
  toJSON ni = object
              [ "id"        .= nodeId ni
              , "addresses" .= nodeAddresses ni
              ]

-----------------------------------------------------------------
-- inserting / fetching data
-----------------------------------------------------------------

class UriFetch a where
  getUriData :: a -> FN.URI -> IO (Either T.Text (BS.ByteString, Int))

  
