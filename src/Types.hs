
module Types (
  NodeInfo(..)
  ) where

import Control.Applicative ( (<$>) )
import Data.Binary
import Data.Binary.IEEE754

data NodeInfo = NodeInfo
                { nodeLocation :: Double
                } deriving ( Show )

instance Binary NodeInfo where
  put ni = putFloat64be $ nodeLocation ni
  get = NodeInfo <$> getFloat64be
