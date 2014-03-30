
{-# LANGUAGE OverloadedStrings #-}

module Node (
  -- * our node
  Node(..),
  
  ) where

import Control.Concurrent ( forkIO, myThreadId, ThreadId )
import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBMQueue as STM
import Control.Monad ( void )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.TQueue as C

import Message as MSG
import Peers
import Types

-------------------------------------------------------------------------
-- Node
-------------------------------------------------------------------------

data Node = Node
            {
            } deriving ( Show )

handleMessage :: (Show a) => Node -> PeerNode a -> MSG.Message a -> IO ()
handleMessage n pn msg = do
  print ("incoming message", n, pn, msg)
  return ()
  
