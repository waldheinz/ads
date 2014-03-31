
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Fproxy (
  fproxy
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Encoding ( encodeUtf8 )
import Network.HTTP.Types.Status
import qualified Network.Wai as WAI

import Freenet
import Freenet.Fetch
import Freenet.URI

-- | error response
er :: T.Text -> WAI.Response
er msg = WAI.responseLBS status500 [] (BSL.fromStrict $ encodeUtf8 msg)

fproxy :: Freenet a -> WAI.Application
fproxy fn req = do
  let
    path = T.intercalate "/" $ WAI.pathInfo req

  case parseUri path of
    Left e    -> return $ er e
    Right uri -> do
      doc <- fetchUri fn uri

      case doc of
        Left e -> return $ er e
        Right bs -> do
          let
            headers = []
            
          return $ WAI.responseLBS status200 headers bs
