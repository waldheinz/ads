
{-# LANGUAGE OverloadedStrings #-}

module Node (
  -- * static Node information
  getNodeInfo,
  
  Node, connectNode, enqMessage,
  
  -- * incoming / outgoig messages
  runNode
  ) where

import Control.Concurrent ( forkIO, myThreadId, ThreadId )
import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBMQueue as STM
import Control.Monad ( void )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as HEX
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit as C
import qualified Data.Conduit.Network as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Serialization.Binary as C
import qualified Data.Conduit.TQueue as C
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG

import Message as MSG
import Types

-------------------------------------------------------------------------
-- NodeInfo related
-------------------------------------------------------------------------

-- | extracts our identity from the config
getNodeInfo :: CFG.Config -> IO NodeInfo
getNodeInfo cfg = do
  nid <- CFG.require cfg "id"
  let (nid', rest) = HEX.decode nid

  return $! NodeInfo nid'

data Node
  = Node
    { nInfo    :: NodeInfo
    , _nThread :: ThreadId
    , nQueue   :: STM.TBMQueue Message -- ^ outgoing message queue
    }

instance Show (Node) where
  show n = "Node {ni = " ++ show (nInfo n) ++ " }"
  
-- | puts a message on the Node's outgoing message queue       
enqMessage :: Node -> MSG.Message -> STM ()
enqMessage n m = writeTBMQueue (nQueue n) m

------------------------------------------------------------------------------
-- Handshake
------------------------------------------------------------------------------

-- ^ handle a node that is currently connected to us
runNode
  :: C.Source IO BS.ByteString
  -> C.Sink BS.ByteString IO ()
  -> Maybe NodeInfo             -- ^ our NodeInfo, if this is an outbound connection
  -> (Node -> IO ())            -- ^ act upon the node once handshake has completed
  -> IO ()
runNode src sink mni connected = do
  mq <- STM.newTBMQueueIO 5 :: IO (STM.TBMQueue Message)
  
  case mni of
    Nothing -> return ()
    Just ni -> atomically $ writeTBMQueue mq (MSG.Hello ni)

  void $ forkIO $ src C.$$ C.conduitDecode C.=$ (C.mapM_ $ \msg -> do
    case msg of
      MSG.Hello ni -> do
        print $ "got a NI: " ++ show ni
        t <- myThreadId
        connected $ Node ni t mq
      x -> print x
                                )
  C.sourceTBMQueue mq C.$= C.conduitEncode C.$$ sink

connectNode
  :: NodeInfo      -- ^ our NodeInfo
  -> (String, Int) -- ^ (host, port)
  -> (Node -> IO ()) -- ^ act when handshake completed
  -> IO ()
connectNode ni (host, port) connected =
  C.runTCPClient (C.clientSettings port $ BSC.pack host) $ \ad -> do
    runNode (C.appSource ad) (C.appSink ad) (Just ni) connected
