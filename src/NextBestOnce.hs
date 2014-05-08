
{-# LANGUAGE MultiParamTypeClasses #-}

module NextBestOnce (
  RoutingInfo, mkRoutingInfo,
  Node(..), route, Result(..)
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent.STM
import Data.Binary
import Data.List ( minimumBy )

import Types

{-
class (Eq i, Show i) => Location i where
  toDouble :: i -> Double
  distance :: i -> i -> Double -- ^ distance between two locations in [0..1)
  distance l1 l2 = min d (1 - d) where
    d = if d1 > d2 then d1 - d2 else d2 - d1
    (d1, d2) = (toDouble l1, toDouble l2)
-}

data RoutingInfo l = RI
                     { _riMarked :: [l]
                     , riTarget  :: l -- ^ where should this message go?
                     }

instance (Show l) => Show (RoutingInfo l) where
  show (RI ms l) = "message to " ++ show l ++ " (marked:" ++ show ms ++ ")"

instance Binary l => Binary (RoutingInfo l) where
  put (RI ms l) = put ms >> put l
  get = RI <$> get <*> get

mkRoutingInfo :: (Location l) => l -> RoutingInfo l
mkRoutingInfo l = RI [] l

-- |
-- already visited the specified location?
marked :: (Location l) => RoutingInfo l -> l -> Bool 
marked (RI ls _) l = l `elem` ls

-- |
-- mark the location as visited and get updated message
mark :: (Location l) => RoutingInfo l -> l -> RoutingInfo l
mark (RI ls t) l
  | l `elem` ls = RI ls t   -- ^ it is possible to mark a message twice, but no use recording it
  | otherwise = RI (l:ls) t

data Node n = Node
              { neighbours        :: STM [n]
              , routingInfo       :: m -> RoutingInfo l
              , updateRoutingInfo :: m -> RoutingInfo l -> m
              , pushPred          :: m -> n -> STM ()
              , popPred           :: m -> STM (Maybe n)
              }

instance (Show l) => Show (Node l m n) where
  show n = "Node {l=" ++ (show $ location n) ++ "}"

data Result l m n = Fail          -- ^ we failed with this message
                  | Forward n m   -- ^ pass on to this peer
                  | Backtrack n m -- ^ 

instance (Show l, Show m, Show n) => Show (Result l m n) where
  show Fail = "Fail"
  show (Forward n m) = "Forward " ++ (show m) ++ " to " ++ show n
  show (Backtrack n m) = "Backtrack " ++ (show m) ++ " to " ++ show n
  
closest :: Node m n -> l -> [n] -> n
closest v t ns = minimumBy (\n1 n2 -> cmp (toLocation n1) (toLocation n2)) ns where
  cmp l1 l2 = compare d1 d2 where (d1, d2) = (absLocDist t l1, absLocDist t l2)
                           
route :: (Location l)
  => Node l m n     -- ^ current node
  -> Maybe n        -- ^ previous node, or Nothing if we're backtracking or we're the originator
  -> m              -- ^ message being routed
  -> STM (Result l m n)
route v prev msg = do
  case prev of
    Nothing -> return ()
    Just p  -> pushPred v msg p

  do
    let
      m = routingInfo v msg
      mm = updateRoutingInfo v msg $ mark m $ toLocation v
      
    s <- filter (\n -> not $ marked m $ toLocation n) <$> neighbours v
  
    if (not . null) s
      then do
        let next = closest v (riTarget m) s
                 
        if absLocDist next (riTarget m) >= absLocDist (toLocation v) (riTarget m)
          then return $ Forward next mm
          else return $ Forward next msg
      else do
      -- check if we can backtrack
      
        p <- popPred v msg
        case p of
          Nothing -> return Fail
          Just pp -> return $ Backtrack pp mm
