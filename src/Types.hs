
module Types (
  NodeInfo(..), Block(..)
  ) where

import Control.Applicative ( (<$>) )
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as HEX
import qualified Data.ByteString.Char8 as BSC

data NodeInfo = NodeInfo
                { nodeId :: BS.ByteString -- ^ the globally unique node ID of 256 bits
                } 

instance Show NodeInfo where
  show ni = "NodeInfo {id=" ++ (BSC.unpack $ HEX.encode (nodeId ni)) ++ "}" 

instance Binary NodeInfo where
  put ni = put $ nodeId ni
  get = NodeInfo <$> get

-- | binary data of fixed size
class Binary a => Block a where
  size :: a -> Int
  
