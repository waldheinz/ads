
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
import Freenet.Store
import Node
import Types

restApi :: ToJSON a => Node a -> WAI.Application
restApi node = mapUrls $
  mount "status"
  (  mount "peers" (connStatus node)
 <|> mount "store"
     (  mount "chk" (storeStatus $ fnChkStore $ nodeFreenet node)
    <|> mount "ssk" (storeStatus $ fnSskStore $ nodeFreenet node)
     )
  ) 
  
connStatus :: ToJSON a => Node a -> WAI.Application
connStatus n = stateJsonResponse $ nodePeers n

storeStatus :: StoreFile f -> WAI.Application
storeStatus sf = stateJsonResponse sf

stateJsonResponse :: ToStateJSON a => a -> WAI.Application
stateJsonResponse o _ = do
  sjson <- atomically $ toStateJSON o
  return $ WAI.responseLBS status200 headers $ encode sjson
  where
    headers = [("Content-Type", "application/json")]
