
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sandbox (
  Sandbox, runSandbox,
  
  -- * Nodes
  randomNode
  ) where

import Control.Monad.RWS.Strict
import System.Random ( StdGen, mkStdGen )

import Node
import Peers
import Types

import Network

newtype Sandbox a = Sandbox { unSandbox :: RWST () () State IO a }
                  deriving ( Monad, MonadState State, MonadIO )
                         
type SandboxNode = Node MsgPassAddress

data State = State
             { stNodes :: ! [SandboxNode]
             , stRng   :: ! StdGen
             }

runSandbox :: Sandbox a -> IO a
runSandbox s = do
  fmap fst $ evalRWST (unSandbox s) () $ State [] (mkStdGen 23)
  
randomNode :: Sandbox SandboxNode
randomNode = do
  nid <- get >>= \s -> let (i, g') = randomId (stRng s) in put s { stRng = g' } >> return i
  let
    ni = NodeInfo nid []
    fn = undefined

  node <- liftIO $ mkNode ni fn undefined undefined
  liftIO . putStrLn $ "created node " ++ show nid
  
  return node
