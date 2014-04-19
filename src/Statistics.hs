
module Statistics (
  -- * Histograms
  Histogram, mkHistogram, histInc
  ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent.STM
import Data.Aeson
import Data.Array.MArray
import Data.Word

import Types

data Histogram = Histogram
                 { histVals :: TArray Int Word64
                 }

instance ToStateJSON Histogram where
  toStateJSON h = do
    assocs <- getAssocs $ histVals h
    return $ toJSON assocs

-- |
-- Create a new Histogram with the specified number of bins.
mkHistogram
  :: Int            -- ^ number of bins
  -> STM Histogram
mkHistogram bins = Histogram <$> newArray (0, bins - 1) 0

-- |
-- Increment the histogram by 1 at the given location.
histInc :: Histogram -> Double -> STM ()
histInc h l = do
  (minBin, maxBin) <- getBounds a
  
  let
    idx = max minBin $ min maxBin $ minBin + (floor $ l * (fromIntegral maxBin + 1))
  
  v <- readArray a idx
  writeArray a idx (v + 1)
  where
    a = histVals h
    
