
module Main ( main ) where

import Control.Monad ( forever )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Serialization.Binary as C
import Data.Conduit.Network
import Network ( withSocketsDo )
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( atomically )

import Message as MSG
import Node as N

handshake :: AppData IO -> IO ()
handshake ad = do
  print $ appSockAddr ad
  n <- atomically $ mkNode
  appSource ad C.$= C.conduitDecode C.$$ C.mapM_ N.handleMessage
  N.nodeOutgoing n C.$= C.conduitEncode C.$$ appSink ad
  
main :: IO ()
main = withSocketsDo $ do
  let s = serverSettings 1234 HostAny
  runTCPServer s handshake
  
