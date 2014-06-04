
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
  , testProperty "loc rightOf" propRightOf
  , testProperty "dist sign" distSign
  , testProperty "arbitrary distance" arbDist
  , testProperty "locMove bounds" locMoveBounds
  , testProperty "undo move" locMoveReverse
  ]

  
distInBounds :: Location -> Location -> Bool
distInBounds l1 l2 = d >= (-1 % 2) && d <= (1 % 2) where
  d = unDistance $ l1 `locDist` l2

propRightOf :: Location -> Location -> Property
propRightOf l1 l2 = l1 /= l2 ==> r1 `xor` r2 where
  r1 = l1 `rightOf` l2
  r2 = l2 `rightOf` l1
  xor p q = (p || q) && not (p && q)

distSign :: Location -> Location -> Bool
distSign l1 l2 = d >= 0 && (l1 `rightOf` l2) || d <= 0 && (l2 `rightOf` l1) where
  d = unDistance $ l1 `locDist` l2

absDistInBounds :: Location -> Location -> Bool
absDistInBounds l1 l2 = d >= 0 && d <= (1 % 2) where
  d = unDistance $ l1 `absLocDist` l2

arbDist :: LocDistance -> Bool
arbDist d = let d' = unDistance d in d' >= (-0.5) && d' <= 0.5

locMoveBounds :: Location -> LocDistance -> Property
locMoveBounds l d = counterexample (show $ (l, unDistance d, l')) $ l' >= 0 && l' < 1 where
  l' = unLocation $ l `locMove` d
  
locMoveReverse :: Location -> LocDistance -> Property
locMoveReverse l d = abs (unDistance d) /= (1 % 2) ==> counterexample
                     ("d=" ++ show (unDistance d) ++ ", d'=" ++ show (unDistance d'))
                     (d' == d)
  where
    l' = l `locMove` d
    d' = locDist l' l


