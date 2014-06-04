
module Statistics (
  -- * Histograms
  Histogram, mkHistogram,
  freezeHistogram, thawHistogram,

  -- ** Transactional Histograms
  THistogram, mkTHistogram, histInc, histDec,

  -- * Scale Free Estimators
  TEstimator, mkTEstimator, updateTEstimator,
  teToList
  ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent.STM
import Data.Aeson
import Data.Binary
import qualified Data.Array.IArray as IA
import Data.Array.MArray

import Types

-----------------------------------------------------------------------
-- Scale Free Estimator
-----------------------------------------------------------------------

data TEstimator = TEstimator
                  { estPoints :: ! (TArray Int (Double, Double))
                  , estFactor :: ! Double
                  }

instance ToStateJSON TEstimator where
  toStateJSON e = do
    assocs <- getAssocs $ estPoints e
    return $ toJSON assocs

mkTEstimator
  :: Int    -- ^ number of data points
  -> Double -- ^ scale factor determining how fast the estimator adapts
  -> Double -- ^ initial value for each data point
  -> STM TEstimator
mkTEstimator cnt f v = do
  arr <- newListArray (0, cnt - 1) [(fromIntegral x / (fromIntegral cnt), v) | x <- [0..(cnt-1)]]
  return $ TEstimator arr f

teToList :: TEstimator -> STM [(Double, Double)]
teToList = getElems . estPoints

updateTEstimator
  :: TEstimator
  -> Location   -- ^ where to update the estimator
  -> Double     -- ^ new value
  -> STM ()
updateTEstimator est loc val = do
  (l,  r) <- teIndices est loc
  
  let
    arr  = estPoints est
    f    = estFactor est
    loc' = fromRational $ unLocation loc
    
  readArray arr l >>= \(x0, y0) ->
    writeArray arr l (x0 + ((loc' - x0) * f), y0 + (val - y0) * f)
    
  readArray arr r >>= \(x1, y1) ->
    writeArray arr r (x1 + ((x1 - loc') * f), y1 + (val - y1) * f)

-- |
-- Finds the (left, right) index pair for the given location.
teIndices :: TEstimator -> Location -> STM (Int, Int)
teIndices est loc = getBounds (estPoints est) >>= \(_, maxIdx) -> go maxIdx 0
  where
    loc' = fromRational $ unLocation loc
    go mi n
      | n == mi = return (mi, 0) -- wrap around
      | otherwise = do
        (x1, _) <- readArray (estPoints est) n
        (x2, _) <- readArray (estPoints est) $ n + 1
        
        if  (x2 < loc') && (loc' < x1)
          then return (n, n + 1)
          else go mi $ n + 1
  
-----------------------------------------------------------------------
-- Histograms
-----------------------------------------------------------------------

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
histInc :: HasLocation l => THistogram -> l -> STM ()
histInc = histMod (\x -> x + 1)

histDec :: HasLocation l => THistogram -> l -> STM ()
histDec = histMod (\x -> x - 1)

histMod :: HasLocation l => (Word64 -> Word64) -> THistogram -> l -> STM ()
histMod f h l = do
  (minBin, maxBin) <- getBounds a
  
  let
    l' = unLocation . toLocation $ l
    idx = max minBin $ min maxBin $ minBin + (floor $ l' * (fromIntegral maxBin + 1))
    
  readArray a idx >>= \v -> writeArray a idx $! f v -- this should be strict
  where
    a = histVals h

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
