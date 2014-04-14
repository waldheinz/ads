
{-# LANGUAGE OverloadedStrings #-}

module RestApi (
  restApi
  ) where

import Control.Applicative ( (<|>) )
import Control.Concurrent.STM
import Data.Aeson
import Network.HTTP.Types ( status200 )
import qualified Network.Wai as WAI
import Network.Wai.UrlMap

import Freenet
import Node
import Types

restApi :: ToJSON a => Node a -> WAI.Application
restApi node = mapUrls $
  mount "status"
  (  mount "peers"   (connStatus node)
 <|> mount "routing" (routeStatus node)   
 <|> mount "store"
     (  mount "chk" (stateJsonResponse $ fnChkStore $ nodeFreenet node)
    <|> mount "ssk" (stateJsonResponse $ fnSskStore $ nodeFreenet node)
     )
  ) 
  
connStatus :: ToJSON a => Node a -> WAI.Application
connStatus n = stateJsonResponse $ nodePeers n

routeStatus :: ToJSON a => Node a -> WAI.Application
routeStatus n r = nodeRouteStatus n >>= \o -> jsonResponse o r

stateJsonResponse :: ToStateJSON a => a -> WAI.Application
stateJsonResponse o req = atomically (toStateJSON o) >>= \o' -> jsonResponse o' req

jsonResponse :: ToJSON a => a -> WAI.Application
jsonResponse o _ = return $ WAI.responseLBS status200 headers $ encode o
  where
    headers = [("Content-Type", "application/json")]
