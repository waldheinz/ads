
{-# LANGUAGE OverloadedStrings #-}

module Freenet.URI (
  URI(..), parseUri, toDataRequest, uriLocation
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
  
import Freenet.Base64
import Freenet.Types  

data URI
     = CHK
       { chkLocation :: Key           -- ^ the routing key
       , chkKey      :: Key           -- ^ the crypto key
       , chkExtra    :: BS.ByteString -- ^ extra data about algorithms used, always 5 bytes
       }
     deriving ( Show )

parseUri :: T.Text -> Either T.Text URI
parseUri str = case T.take 4 str of
  "CHK@" -> parseChk (T.drop 4 str)
  _      -> Left $ T.concat ["cannot recognize URI type of \"", str, "\""]

parseChk :: T.Text -> Either T.Text URI
parseChk str = case T.split (== ',') str of
  [rstr, cstr, estr] -> do
    rk <- fromBase64' rstr >>= mkKey
    ck <- fromBase64' cstr >>= mkKey
    e <- fromBase64' estr >>= \eb -> if BS.length eb == 5
                                     then Right $ eb
                                     else Left "CHK extra data must be 5 bytes"
    return $ CHK rk ck e
  _ -> Left $ T.concat $ ["expected 3 comma-separated parts in \"", str, "\""]
  
toDataRequest :: URI -> DataRequest
toDataRequest (CHK loc _ e) = ChkRequest loc $ BS.index e 1

uriLocation :: URI -> Key
uriLocation (CHK loc _ _) = loc
