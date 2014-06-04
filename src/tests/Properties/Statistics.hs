
module Properties.Statistics ( statsTests ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent.STM
import Data.List ( sort )
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Statistics
import Types

statsTests :: [Test]
statsTests =
  [ testProperty "estimator sorted" estSorted
  ]

estSorted :: [(Location, Double)] -> Property
estSorted input = monadicIO $ run $ atomically $ do
  est <- mkTEstimator 10 0.1 0.0
  mapM_ (uncurry (updateTEstimator est)) input
  el <- map fst <$> teToList est
  return $ sort el == el
