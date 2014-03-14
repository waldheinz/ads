
module Node (
  Node, mkNode, nodeOutgoing,
  
  -- * incoming / outgoig messages
  handleMessage
  ) where

import Control.Applicative ( (<$>) )
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBMQueue as STM
import Control.Monad.IO.Class ( MonadIO )
import qualified Data.Conduit as C
import qualified Data.Conduit.TQueue as C

import Message as MSG

data Node = Node
            { nMsgQ :: STM.TBMQueue Message -- ^ outgoing message queue
            }

mkNode :: STM.STM Node
mkNode = do
  mq <- STM.newTBMQueue 5
  return $ Node mq

nodeOutgoing :: MonadIO m => Node -> C.Source m MSG.Message
nodeOutgoing n = C.sourceTBMQueue $ nMsgQ n

handleMessage :: MSG.Message -> IO ()
handleMessage msg = do
  print msg
