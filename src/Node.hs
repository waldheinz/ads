
{-# LANGUAGE OverloadedStrings #-}

module Node (
  Node, nodePeer, enqMessage,
  
  -- * incoming / outgoig messages
  runNode
  ) where

import Control.Concurrent ( forkIO, myThreadId, ThreadId )
import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBMQueue as STM
import Control.Monad ( void )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.TQueue as C

import Message as MSG
import Types

-------------------------------------------------------------------------
-- Nodes
-------------------------------------------------------------------------

data Node a
  = Node
    { nodePeer :: Peer a
    , _nThread :: ThreadId
    , nQueue   :: STM.TBMQueue (Message a) -- ^ outgoing message queue
    }

instance (Show a) => Show (Node a) where
  show n = "Node {peer = " ++ show (nodePeer n) ++ " }"
  
-- | puts a message on the Node's outgoing message queue       
enqMessage :: Node a -> MSG.Message a -> STM ()
enqMessage n m = writeTBMQueue (nQueue n) m

------------------------------------------------------------------------------
-- Handshake
------------------------------------------------------------------------------

-- ^ handle a node that is currently connected to us
runNode
  :: (Show a)
  => C.Source IO (Message a)
  -> C.Sink (Message a) IO ()
  -> Maybe (Peer a)           -- ^ our NodeInfo, if this is an outbound connection
  -> (Node a -> IO ())        -- ^ act upon the node once handshake has completed
  -> IO ()
runNode src sink mni connected = do
  mq <- STM.newTBMQueueIO 5 :: IO (STM.TBMQueue (Message a))
  
  case mni of
    Nothing -> return ()
    Just ni -> atomically $ writeTBMQueue mq (MSG.Hello ni)

  void $ forkIO $ src C.$$ (C.mapM_ $ \msg -> do
    case msg of
      MSG.Hello p -> do
        print $ "got a Peer: " ++ show p
        t <- myThreadId
        connected $ Node p t mq
      x -> print x
                                )
  C.sourceTBMQueue mq C.$$ sink
