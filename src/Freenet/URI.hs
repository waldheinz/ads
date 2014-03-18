
{-# LANGUAGE OverloadedStrings #-}

module Freenet.URI (
  URI, parseURI
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
  
import Freenet.Base64
  
newtype Key = Key BS.ByteString deriving ( Show )
                                         
mkKey :: BS.ByteString -> Either T.Text Key
mkKey bs = if BS.length bs == 32
           then Right $ Key bs
           else Left  $ "keys must be 32 bytes"

data URI
     = CHK
       { chkLocation :: Key -- ^ the routing key
       , chkKey      :: Key -- ^ the crypto key
       , chkExtra    :: BS.ByteString -- ^ extra data about algorithms used, always 5 bytes
       }
     deriving ( Show )

parseURI :: T.Text -> Either T.Text URI
parseURI str = case T.take 4 str of
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
  
