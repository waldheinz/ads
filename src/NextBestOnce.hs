
{-# LANGUAGE MultiParamTypeClasses #-}

module NextBestOnce (
  Location(..), RoutingInfo, Message(..),
  Node(..), route, Result(..)
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent.STM
import Data.Binary
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List ( minimumBy )

class (Eq i, Show i) => Location i where
  distance :: i -> i -> Double -- ^ distance between two locations in [0..1)


data RoutingInfo l = RI
                     { riId      :: Int -- ^ message id for finding predecessors
                     , _riMarked :: [l]
                     , riTarget  :: l   -- ^ where should this message go?
                     }

instance (Show l) => Show (RoutingInfo l) where
  show (RI _ _ l) = "message to " ++ show l

instance Binary l => Binary (RoutingInfo l) where
  put (RI mid ms l) = put mid >> put ms >> put l
  get = RI <$> get <*> get <*> get

-- |
-- A message carrying a RoutingInfo
class (Location l) => Message m l where
  routingInfo       :: m -> RoutingInfo l
--  updateRoutingInfo :: m -> RoutingInfo m -> m

-- |
-- already visited the specified location?
marked :: (Location l) => RoutingInfo l -> l -> Bool 
marked (RI _ ls _) l = l `elem` ls

-- |
-- mark the location as visited and get updated message
mark :: (Location l) => RoutingInfo l -> l -> RoutingInfo l
mark (RI mid ls t) l = RI mid (l:ls) t

--type ActiveMessage m l  = (Message m, RoutingInfo l)

data Node l m = Node
              { location   :: l
              , success    :: RoutingInfo l -> STM Bool
              , neighbours :: STM [Node l m]
              , msgPreds   :: TVar (IntMap m)
--              , addPred    :: Node l  -> STM ()
--              , popPred    :: STM (Maybe (Node l))
              }

addPred :: Node l m -> Node l m -> RoutingInfo l -> STM ()
addPred self n ri = undefined -- modifyTVar' (msgPreds self) $ IntMap.insertWith (++) (riId ri) [n]

popPred :: Node l m -> RoutingInfo l -> STM (Maybe (Node l m))
popPred self ro = undefined -- modifyTVar' (msgPreds self) $

instance (Show l) => Show (Node l m) where
  show n = "Node {l=" ++ (show $ location n) ++ "}"

data Result l m = Fail                                 -- ^ we failed with this message
                | Success                              -- ^ the message was dealt with
                | Forward (Node l m) (RoutingInfo l)   -- ^ pass on to this peer
                | Backtrack (Node l m) (RoutingInfo l) -- ^ 

instance (Show l) => Show (Result l m) where
  show Fail = "Fail"
  show Success = "Success"
  show (Forward n m) = "Forward " ++ (show m) ++ " to " ++ show n
  show (Backtrack n m) = "Backtrack " ++ (show m) ++ " to " ++ show n
  
closest :: (Location l) => l -> [Node l m] -> Node l m
closest t ns = minimumBy (\n1 n2 -> cmp (location n1) (location n2)) ns where
  cmp l1 l2 = compare d1 d2 where (d1, d2) = (distance t l1, distance t l2)
                           
route :: (Location l)
  => Node l m       -- ^ current node
  -> Maybe (Node l m) -- ^ previous node, or Nothing if we're backtracking or we're the originator
  -> RoutingInfo l  -- ^ message being routed
  -> STM (Result l m)
  
route v prev m = success v m >>= \done ->
  if done
  then return Success
  else do
    case prev of
      Nothing -> return ()
      Just p  -> addPred v p m

    s <- filter (\n -> not $ marked m $ location n) <$> neighbours v
    
    let
      mm = mark m $ location v
      
    if (not . null) s
      then do
        let next = closest (riTarget m) s
                 
        if distance (location next) (riTarget m) >= distance (location v) (riTarget m)
          then return $ Forward next mm
          else return $ Forward next m
      else do
        -- check if we can backtrack
      
        p <- popPred v m
        case p of
          Nothing -> return Fail
          Just pp -> return $ Backtrack pp mm
