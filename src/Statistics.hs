
module Statistics (
  -- * Transactional Histograms
  THistogram, mkTHistogram, histInc,

  -- * Histograms
  Histogram, mkHistogram,
  freezeHistogram, thawHistogram
  ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent.STM
import Data.Aeson
import Data.Binary
import qualified Data.Array.IArray as IA
import Data.Array.MArray

import Types

data THistogram = THistogram
                 { histVals :: TArray Int Word64
                 }

instance ToStateJSON THistogram where
  toStateJSON h = do
    assocs <- getAssocs $ histVals h
    return $ toJSON assocs

-- |
-- Create a new Histogram with the specified number of bins.
mkTHistogram
  :: Int            -- ^ number of bins
  -> STM THistogram
mkTHistogram bins = THistogram <$> newArray (0, bins - 1) 0

-- |
-- Increment the histogram by 1 at the given location.
histInc :: THistogram -> Double -> STM ()
histInc h l = do
  (minBin, maxBin) <- getBounds a
  
  let
    idx = max minBin $ min maxBin $ minBin + (floor $ l * (fromIntegral maxBin + 1))
  
  v <- readArray a idx
  writeArray a idx (v + 1)
  where
    a = histVals h
    
----------------------------------------------------------------------------------------
-- Histograms
----------------------------------------------------------------------------------------

newtype Histogram = Histogram { unHistogram :: IA.Array Int Word64 }

instance Binary Histogram where
  put (Histogram arr) = put arr
  get = Histogram <$> get

mkHistogram :: Int -> Histogram
mkHistogram bins = Histogram $ IA.listArray (0, bins - 1) $ repeat 0

freezeHistogram :: THistogram -> STM Histogram
freezeHistogram th = Histogram <$> (freeze . histVals) th

thawHistogram :: Histogram -> STM THistogram
thawHistogram h = THistogram <$> (thaw . unHistogram) h
