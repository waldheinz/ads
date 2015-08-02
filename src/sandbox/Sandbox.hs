
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sandbox (
  Sandbox, runSandbox, delay,

  -- * Nodes
  randomNode, addPeer
  ) where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.STM
import Control.Monad.RWS.Strict
import System.Random ( StdGen, mkStdGen )

import Logging
import Network
import Node
import Types

newtype Sandbox a = Sandbox { unSandbox :: RWST Environment () State IO a }
                  deriving ( Applicative, Functor, Monad, MonadReader Environment, MonadState State, MonadIO )

type SandboxNode = Node MsgPassAddress

data Environment = Env
                   { envNet :: ! MsgPassNet
                   }

data State = State
             { stNodes :: ! [SandboxNode]
             , stRng   :: ! StdGen
             }

runSandbox :: Sandbox a -> IO a
runSandbox s = do
  net <- mkMsgPassNet
  initLogging

  let
    env = Env net
    st  = State [] (mkStdGen 23)

  fmap fst $ evalRWST (unSandbox s) env st

delay :: Int -> Sandbox ()
delay d = liftIO $ threadDelay d

liftSTM :: STM a -> Sandbox a
liftSTM = liftIO . atomically

randomNode :: Sandbox SandboxNode
randomNode = do
  nid  <- get >>= \s -> let (i, g') = randomId (stRng s) in put s { stRng = g' } >> return i
  node <- liftIO $ mkNode (NodeInfo nid [])
  na   <- reader envNet >>= liftIO . (mkAddress node)
  liftSTM $ modifyTVar' (nodeIdentity node) $ \oid -> oid { nodeAddresses = [na] }
  liftIO . putStrLn $ "created node " ++ show nid
  return node

addPeer
  :: SandboxNode -- ^ the node whose peer list gets the other node added
  -> SandboxNode -- ^ the node to announce to the first node
  -> Sandbox ()
addPeer n1 n2 = liftIO $ atomically $ do
  oid <- readTVar $ nodeIdentity n2
  mergeNodeInfo n1 oid
