
module Properties.Types ( typeTests ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.Ratio

import Types

typeTests :: [Test]
typeTests =
  [ testProperty "distance in bounds" distInBounds
  , testProperty "abs dist in bounds" absDistInBounds
  ]
  
distInBounds :: Location -> Location -> Bool
distInBounds l1 l2 = d >= (-1 % 2) && d <= (1 % 2) where
  d = unDistance $ l1 `locDist` l2

absDistInBounds :: Location -> Location -> Bool
absDistInBounds l1 l2 = d >= 0 && d <= (1 % 2) where
  d = unDistance $ l1 `absLocDist` l2
