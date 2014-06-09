
{-# LANGUAGE OverloadedStrings #-}

module RestApi (
  startRestApi
  ) where

import Control.Applicative ( (<|>), (<$>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void )
import Data.Aeson
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import Data.String ( fromString )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Network.HTTP.Types ( status200, status400 )
import qualified Network.Wai as WAI
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Parse as WAI
import Network.Wai.UrlMap

import Freenet.Insert
import Node
import Peers
import Types
import Utils

startRestApi :: (PeerAddress a, ToJSON a) => CFG.Config -> Node a -> IO ()
startRestApi cfg node = do
  host <- CFG.lookup cfg "host"
  port <- CFG.lookupDefault 8080 cfg "port"
  
  case host of
    Nothing -> return ()
    Just h  -> do
      void $ forkIO $ Warp.runSettings
        (Warp.setHost (fromString h) $ Warp.setPort port $ Warp.defaultSettings)
        (restApi node)

restApi :: (PeerAddress a, ToJSON a) => Node a -> WAI.Application
restApi node = mapUrls $
  mount "api"
    ( mount "fetch"
     (  mount "chk" (fetchChk node)
    <|> mount "ssk" (fetchSsk node)   
     )
   <|> mount "insert"
      ( mount "file" (insertFile node)
      )  
   <|> mount "status"
      (  mount "peers"   (connStatus node)
     <|> mount "routing" (routeStatus node)   
     <|> mount "store"
        (--  mount "chk" (stateJsonResponse $ nodeChkStore node)
--       <|> mount "ssk" (stateJsonResponse $ nodeSskStore node)
        )
      )
    )  
 <|> mountRoot webUi

webUi :: WAI.Application
webUi = staticApp $ defaultFileServerSettings "./webUi"
  
connStatus :: ToJSON a => Node a -> WAI.Application
connStatus n r = nodeConnectStatus n >>= \o -> jsonResponse o r

routeStatus :: ToJSON a => Node a -> WAI.Application
routeStatus n r = nodeRouteStatus n >>= \o -> jsonResponse o r

stateJsonResponse :: ToStateJSON a => a -> WAI.Application
stateJsonResponse o req = atomically (toStateJSON o) >>= \o' -> jsonResponse o' req

jsonResponse :: ToJSON a => a -> WAI.Application
jsonResponse o _ = return $ WAI.responseLBS status200 headers $ encode o
  where
    headers = [("Content-Type", "application/json")]

badRequest :: T.Text -> WAI.Application
badRequest msg _ = return $ WAI.responseLBS status400 headers $ bsFromStrict $ encodeUtf8 msg
  where
    headers = [("Content-Type", "text/plain; charset=utf-8")]

fetchChk :: PeerAddress a => Node a -> WAI.Application
fetchChk node req = do
  chkReq <- eitherDecode <$> WAI.lazyRequestBody req
  
  case chkReq of
    Left e  -> badRequest (T.pack e) req
    Right r -> nodeFetchChk node r $ \result -> do
      case result of
        Left e    -> badRequest e req
        Right blk -> jsonResponse blk req
        
fetchSsk :: PeerAddress a => Node a -> WAI.Application
fetchSsk node req = do
  chkReq <- eitherDecode <$> WAI.lazyRequestBody req
  
  case chkReq of
    Left e  -> badRequest (T.pack e) req
    Right r -> nodeFetchSsk node r $ \result -> do
      case result of
        Left e    -> badRequest e req
        Right blk -> jsonResponse blk req

--insertBackend :: Node a -> WAI.BackEnd (Sink 
--insertBackend node pn fi = do
--  (bsrc, bsink) <- liftIO $ CU.pairTQueue
--  prog <- liftIO $ void $ liftIO $ insert InsertCHK (InsertDirect bsrc (decodeUtf8 $ WAI.fileContentType fi))
--  return bsink
        
insertFile :: PeerAddress a => Node a -> WAI.Application
insertFile node req = do
  case WAI.getRequestBodyType req of
    Nothing -> badRequest "must post url encoded or multipart data here" req
    Just _ -> do
      (_, files) <- WAI.parseRequestBody WAI.lbsBackEnd req
      uris <- mapM (\(_, fi) -> insert node InsertCHK (InsertDirect (WAI.fileContent fi) (decodeUtf8 $ WAI.fileContentType fi))) files
      badRequest (T.pack $ show uris) req
      
