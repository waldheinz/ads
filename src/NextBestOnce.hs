
{-# LANGUAGE MultiParamTypeClasses #-}

module NextBestOnce (
  Location(..),
  RoutingInfo, mkRoutingInfo,
  Node(..), route, Result(..)
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent.STM
import Data.Binary
import Data.List ( minimumBy )

class (Eq i, Show i) => Location i where
  toDouble :: i -> Double
  distance :: i -> i -> Double -- ^ distance between two locations in [0..1)
  distance l1 l2
    | d1 > d2 = d1 - d2
    | otherwise = d2 - d1
    where
      (d1, d2) = (toDouble l1, toDouble l2)

data RoutingInfo l = RI
                     { _riMarked :: [l]
                     , riTarget  :: l   -- ^ where should this message go?
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
mark (RI ls t) l = RI (l:ls) t

data Node l m n = Node
                  { location          :: l
                  , neighbours        :: STM [n]
                  , neighbourLocation :: n -> l
                  , routingInfo       :: m -> RoutingInfo l
                  , updateRoutingInfo :: m -> RoutingInfo l -> m
                  , addPred           :: m -> n -> STM ()
                  , popPred           :: m -> STM (Maybe n)
                  }

instance (Show l) => Show (Node l m n) where
  show n = "Node {l=" ++ (show $ location n) ++ "}"

data Result l m n = Fail          -- ^ we failed with this message
--                  | Success       -- ^ the message was dealt with
                  | Forward n m   -- ^ pass on to this peer
                  | Backtrack n m -- ^ 

instance (Show l, Show m, Show n) => Show (Result l m n) where
  show Fail = "Fail"
--  show Success = "Success"
  show (Forward n m) = "Forward " ++ (show m) ++ " to " ++ show n
  show (Backtrack n m) = "Backtrack " ++ (show m) ++ " to " ++ show n
  
closest :: (Location l) => Node l m n -> l -> [n] -> n
closest v t ns = minimumBy (\n1 n2 -> cmp (neighbourLocation v n1) (neighbourLocation v n2)) ns where
  cmp l1 l2 = compare d1 d2 where (d1, d2) = (distance t l1, distance t l2)
                           
route :: (Location l)
  => Node l m n     -- ^ current node
  -> Maybe n        -- ^ previous node, or Nothing if we're backtracking or we're the originator
  -> m              -- ^ message being routed
  -> STM (Result l m n)
  
route v prev msg = do
    case prev of
      Nothing -> return ()
      Just p  -> addPred v msg p
    
    let
      m = routingInfo v msg
      mm = updateRoutingInfo v msg $ mark m $ location v
      
    s <- filter (\n -> not $ marked m $ neighbourLocation v n) <$> neighbours v
      
    if (not . null) s
      then do
        let next = closest v (riTarget m) s
                 
        if distance (neighbourLocation v next) (riTarget m) >= distance (location v) (riTarget m)
          then return $ Forward next mm
          else return $ Forward next msg
      else do
        -- check if we can backtrack
      
        p <- popPred v msg
        case p of
          Nothing -> return Fail
          Just pp -> return $ Backtrack pp mm
