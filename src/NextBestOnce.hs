
module NextBestOnce (
  Location(..), RoutingInfo, Node(..), route, Result(..)
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent.STM
import Data.Binary
import Data.List ( minimumBy )

class (Eq i, Show i) => Location i where
  distance :: i -> i -> Double -- ^ distance between two locations in [0..1)

instance Location Int where
  distance l1 l2 = (fromIntegral $ abs (l1 - l2)) / fromIntegral (maxBound :: Int)

data RoutingInfo l = RI
                     { _riMarked :: [l]
                     , riTarget :: l   -- ^ where should this message go?
                     }

instance (Show l) => Show (RoutingInfo l) where
  show (RI _ l) = "message to " ++ show l

instance Binary l => Binary (RoutingInfo l) where
  put (RI ms l) = put ms >> put l
  get = RI <$> get <*> get

-- |
-- already visited the specified location?
marked :: (Location l) => RoutingInfo l -> l -> Bool 
marked (RI ls _) l = l `elem` ls

-- |
-- mark the location as visited and get updated message
mark :: (Location l) => RoutingInfo l -> l -> RoutingInfo l
mark (RI ls t) l = RI (l:ls) t

data Node l = Node
              { location   :: l
              , success    :: RoutingInfo l -> STM Bool
              , neighbours :: STM [Node l]
              , addPred    :: (Node l) -> STM ()
              , popPred    :: STM (Maybe (Node l))
              }

instance (Show l) => Show (Node l) where
  show n = "Node {l=" ++ (show $ location n) ++ "}"

data Result l = Fail                               -- ^ we failed with this message
              | Success                            -- ^ the message was dealt with
              | Forward (Node l) (RoutingInfo l)   -- ^ pass on to this peer
              | Backtrack (Node l) (RoutingInfo l) -- ^ 

instance (Show l) => Show (Result l) where
  show Fail = "Fail"
  show Success = "Success"
  show (Forward n m) = "Forward " ++ (show m) ++ " to " ++ show n
  show (Backtrack n m) = "Backtrack " ++ (show m) ++ " to " ++ show n
  
closest :: (Location l) => l -> [Node l] -> Node l
closest t ns = minimumBy (\n1 n2 -> cmp (location n1) (location n2)) ns where
  cmp l1 l2 = compare d1 d2 where (d1, d2) = (distance t l1, distance t l2)
                           
route :: (Location l)
  => Node l         -- ^ current node
  -> Maybe (Node l) -- ^ previous node, or Nothing if we're backtracking or we're the originator
  -> RoutingInfo l  -- ^ message being routed
  -> STM (Result l)
  
route v prev m = success v m >>= \done ->
  if done
  then return Success
  else do
    case prev of
      Nothing -> return ()
      Just p  -> addPred v p

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
      
        p <- popPred v
        case p of
          Nothing -> return Fail
          Just pp -> return $ Backtrack pp mm
