
module Properties.Statistics ( statsTests ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent.STM
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Statistics
import Types

statsTests :: [Test]
statsTests =
  [ testProperty "estimator sorted" estSorted
  , testProperty "est. loc in bounds" estLocInBounds
  ]

estSorted :: [(Location, Double)] -> Property
estSorted input = monadicIO $ do
  el <- run $ atomically $ do
    est <- mkTEstimator 32 0.2 0.0
    mapM_ (uncurry (updateTEstimator est)) input
    map fst <$> teToList est

  let
    locs = map mkLocation el
    right = zipWith rightOf (tail locs) locs

  assert $ and right

estLocInBounds :: [(Location, Double)] -> Property
estLocInBounds input = monadicIO $ do
  el <- run $ atomically $ do
    est <- mkTEstimator 32 0.2 0.0
    mapM_ (uncurry (updateTEstimator est)) input
    map fst <$> teToList est

  assert $ minimum el >= 0.0
  assert $ maximum el < 1.0
