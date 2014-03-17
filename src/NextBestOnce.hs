
{-# LANGUAGE MultiParamTypeClasses #-}

module NextBestOnce (
  Location, Message, Node(..), route
  ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( unless )
import Control.Concurrent.STM

class (Eq i) => Location i where
  distance :: i -> i -> Double -- ^ distance between two locations in [0..1)

instance Location Int where
  distance l1 l2 = (fromIntegral $ abs (l1 - l2)) / (fromIntegral (minBound :: Int) - fromIntegral (maxBound :: Int))

class (Location l) => Message m l where
  marked :: m -> l -> Bool     -- ^ already visited the specified location?
  mark   :: m -> l -> m        -- ^ mark the location as visited and get updated message
  target :: m -> l             -- ^ where should this message go?

instance Message ([Int]) Int where
  --target (t, _) = t
  

data Node m l = Node
                { location   :: l
                , success    :: m -> STM Bool
                , neighbours :: STM [Node m l]
                , addPred    :: (Node m l) -> STM ()
                , popPred    :: STM (Maybe (Node m l))
                }
                  
data Result i = Fail       -- ^ we failed with this message
              | Success    -- ^ the message was dealt with
              | Forward i  -- ^ pass on to this peer

closest :: (Message m l) => m -> [Node m l] -> Node m l
closest = undefined

route :: (Location l, Message m l)
  => Node m l           -- ^ current node
  -> Node m l           -- ^ previous node
  -> m           -- ^ message being routed
  -> Bool        -- ^ backtracking ?
  -> STM (Result (Node m l, m))
  
route v p m b = success v m >>= \done ->
  if done
  then return Success
  else do
    unless b $ addPred v p
    s <- filter (\n -> not $ marked m $ location n) <$> neighbours v
    
    let
      mm = mark m $ location v
      
    (n', m') <- if (not . null) s
                then let next = closest m s in
                  if distance (location next) (target m) >= distance (location v) (target m)
                  then return (Just next, mm)
                  else return (Just next, m)
                     
                else do
                  x <- popPred v
                  return (x, mm)

    case n' of
      Nothing   -> return Fail
      Just next -> return $ Forward (next, m')
