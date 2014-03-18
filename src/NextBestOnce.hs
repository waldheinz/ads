
{-# LANGUAGE MultiParamTypeClasses #-}

module NextBestOnce (
  Location(..), IntMsg(..), Message(..), Node(..), route, Result(..)
  ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent.STM
import Data.List ( minimumBy )

class (Eq i, Show i) => Location i where
  distance :: i -> i -> Double -- ^ distance between two locations in [0..1)

instance Location Int where
  distance l1 l2 = (fromIntegral $ abs (l1 - l2)) / fromIntegral (maxBound :: Int)

class (Location l) => Message m l where
  marked :: (Location l) => m -> l -> Bool     -- ^ already visited the specified location?
  mark   :: (Location l) => m -> l -> m        -- ^ mark the location as visited and get updated message
  target :: (Location l) => m -> l             -- ^ where should this message go?

newtype IntMsg = IntMsg (Int, [Int])

instance Message IntMsg Int where
  target (IntMsg (t, _))    = t
  marked (IntMsg (_, ms)) l = elem l ms
  mark   (IntMsg (t, ms)) l = IntMsg (t, l:ms)

instance Show IntMsg where
  show (IntMsg (t, _)) = "Message {target=" ++ (show t) ++ "}"

data Node m l = Node
                { location   :: l
                , success    :: m -> STM Bool
                , neighbours :: STM [Node m l]
                , addPred    :: (Node m l) -> STM ()
                , popPred    :: STM (Maybe (Node m l))
                }

instance (Show l) => Show (Node m l) where
  show n = "Node {l=" ++ (show $ location n) ++ "}"
                
data Result n m = Fail         -- ^ we failed with this message
                | Success      -- ^ the message was dealt with
                | Forward n m  -- ^ pass on to this peer
                | Backtrack n m  -- ^ 

instance (Show m, Show n) => Show (Result n m) where
  show Fail = "Fail"
  show Success = "Success"
  show (Forward n m) = "Forward " ++ (show m) ++ " to " ++ show n
  show (Backtrack n m) = "Backtrack " ++ (show m) ++ " to " ++ show n
  
closest :: (Message m l) => m -> [Node m l] -> Node m l
closest m ns = minimumBy (\n1 n2 -> cmp (location n1) (location n2)) ns where
  cmp l1 l2 = compare d1 d2 where (d1, d2) = (distance t l1, distance t l2)
  t = target m
                           
route :: (Location l, Message m l)
  => Node m l         -- ^ current node
  -> Maybe (Node m l) -- ^ previous node, or Nothing if we're backtracking or we're the originator
  -> m                -- ^ message being routed
  -> STM (Result (Node m l) m)
  
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
        let next = closest m s
                 
        if distance (location next) (target m) >= distance (location v) (target m)
          then return $ Forward next mm
          else return $ Forward next m
      else do
        -- check if we can backtrack
      
        p <- popPred v
        case p of
          Nothing -> return Fail
          Just pp -> return $ Backtrack pp mm
