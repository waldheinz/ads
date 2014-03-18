
module NextBestOnceTest ( main ) where

import Control.Concurrent.STM

import NextBestOnce

type INode = Node IntMsg Int

data NodeState = NS
                { loc        :: Int
                , nh         :: [NodeState] -- ^ neighbourhood
                , preds      :: TVar [INode]
                }

instance Show NodeState where
  show (NS l ns _) = show (l, ns)

mkNs :: Int -> [NodeState] -> STM NodeState
mkNs l h = newTVar [] >>= \ps -> return $ NS l h ps

mkNode :: NodeState -> INode
mkNode ns = Node
            (loc ns)
            (\m -> return $ target m == loc ns)
            (return $ map mkNode (nh ns))
            (\p -> modifyTVar (preds ns) (\l -> (p:l)))
            (readTVar (preds ns) >>= (\ps -> case ps of
                [] -> return Nothing
                (p:ps') -> writeTVar (preds ns) ps' >> return (Just p))   )
            
main :: IO ()
main = do
  n1 <- atomically $ mkNs 1 []
  n2 <- atomically $ mkNs 2 []
  n3 <- atomically $ mkNs 3 [n1]
  n4 <- atomically $ mkNs 4 [n2, n3]
  n5 <- atomically $ mkNs 5 [n4, n2]
  
  let
    m = IntMsg (1, [])
    nextStep curr (Forward n m') = do
      r <- atomically $ route n (Just curr) m'
      print r
      nextStep n r
    nextStep _ (Backtrack n m') = do
      r <- atomically $ route n Nothing m'
      print r
      nextStep n r
    nextStep curr x = print $ (show x) ++ " at " ++ show curr

  r <- atomically $ route (mkNode n5) Nothing m
  print r
  nextStep (mkNode n5) r
  
  return ()
