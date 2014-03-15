
module Main ( main ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM
import Data.Conduit.Network
import Data.Binary
import Network ( withSocketsDo )

import Node as N
import Peers as P

main :: IO ()
main = withSocketsDo $ do
  let
    s =  -- serverAfterBind (\s -> print s) $ 
      serverSettings 1234 HostAny
    ni = NodeInfo 0.5

  print ("ni", (decode . encode $ ni) :: NodeInfo)
    
  p <- atomically mkPeers

  forkIO $ runTCPServer s $ \ad -> do
      print $ ("client connected", appSockAddr ad)
      N.runNode (appSource ad) (appSink ad) Nothing $ \n -> do
        print n
        atomically $ P.addPeer p n

  threadDelay (1000 * 500)

  forkIO $ N.connectNode ni ("127.0.0.1", 1234) $ \n -> do
    print n

  threadDelay (1000 * 1000 * 100)
