
{-# LANGUAGE MultiParamTypeClasses #-}

module NextBestOnce (
  Routable(..),
  Node(..), route, Result(..)
  ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent.STM
import Data.List ( minimumBy )

import Types

class HasLocation l => Routable m l where
  marked :: m -> l -> Bool -- ^ is the location already marked?
  mark   :: m -> l -> m    -- ^ mark the specified location
  target :: m -> l         -- ^ where should the message be routed

data Node l m n = Node
                  { neighbours        :: STM [n]
                  , selfLocation      :: l
                  , neighbourLocation :: n -> l
                  , pushPred          :: m -> n -> STM ()
                  , popPred           :: m -> STM (Maybe n)
                  }

data Result m n = Fail          -- ^ we failed with this message
                | Forward n m   -- ^ pass on to this peer
                | Backtrack n m -- ^ 

instance (Show m, Show n) => Show (Result m n) where
  show Fail            = "Fail"
  show (Forward n m)   = "Forward " ++ (show m) ++ " to " ++ show n
  show (Backtrack n m) = "Backtrack " ++ (show m) ++ " to " ++ show n

route :: (Routable m l)
  => Node l m n     -- ^ current node
  -> Maybe n        -- ^ previous node, or Nothing if we're backtracking or we're the originator
  -> m              -- ^ message being routed
  -> STM (Result m n)
route v prev msg = do
  case prev of
    Nothing -> return ()
    Just p  -> pushPred v msg p
  
  do
    let
      m = msg
      mm = mark m $ selfLocation v
      tgt = toLocation $ ((target msg) `asTypeOf` (selfLocation v))
      closest ns = minimumBy (\n1 n2 -> cmp (toLocation $ neighbourLocation v n1) (toLocation $ neighbourLocation v $ n2)) ns where
        cmp l1 l2 = compare d1 d2 where (d1, d2) = (absLocDist tgt l1, absLocDist tgt l2)

    s <- filter (\n -> not $ marked m (neighbourLocation v n)) <$> neighbours v
  
    if (not . null) s
      then do
        let next = closest s
                 
        if absLocDist (toLocation $ neighbourLocation v next) tgt >= absLocDist (toLocation $ selfLocation v) tgt
          then return $ Forward next mm
          else return $ Forward next msg
      else do
      -- check if we can backtrack
      
        p <- popPred v msg
        case p of
          Nothing -> return Fail
          Just pp -> return $ Backtrack pp mm
