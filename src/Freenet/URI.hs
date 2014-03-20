
{-# LANGUAGE OverloadedStrings #-}

module Freenet.URI (
  URI(..), parseUri, toDataRequest, uriLocation, mkChkExtra
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSB
import Data.Monoid
import qualified Data.Text as T
import Data.Word
  
import Freenet.Base64
import Freenet.Types  

data URI
     = CHK
       { chkLocation :: Key      -- ^ the routing key
       , chkKey      :: Key      -- ^ the crypto key
       , chkExtra    :: ChkExtra -- ^ extra data about algorithms used, always 5 bytes
       }

instance Show URI where
  show (CHK l k e) = "CHK@" ++ show l ++ "," ++ show k ++ "," ++ show e

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
    return $ CHK rk ck (ChkExtra e)
  _ -> Left $ T.concat $ ["expected 3 comma-separated parts in \"", str, "\""]

toDataRequest :: URI -> DataRequest
toDataRequest (CHK loc _ e) = ChkRequest loc $ chkExtraCrypto e

uriLocation :: URI -> Key
uriLocation (CHK loc _ _) = loc

--------------------------------------------------------------------------------------
-- CHK extra data (last URI component)
--------------------------------------------------------------------------------------

newtype ChkExtra = ChkExtra { unChkExtra :: BS.ByteString }

instance Show ChkExtra where
  show (ChkExtra bs) = T.unpack $ toBase64' bs

-- | construct CHK extra data
mkChkExtra
  :: Word8         -- ^ crypto algorithm
  -> Word16        -- ^ comptression algorithm
  -> Bool          -- ^ control document
  -> ChkExtra      -- ^ resulting 5 bytes of CHK key "extra" data
mkChkExtra crypt compr contr = ChkExtra $ BSL.toStrict $
  BSB.toLazyByteString $ BSB.word8 0 <> BSB.word8 crypt <> BSB.word8 ctrl <> BSB.word16BE compr 
  where
    ctrl = if contr then 2 else 0

-- | extract the crypto algorithm used from an ChkExtra
chkExtraCrypto :: ChkExtra -> Word8
chkExtraCrypto ce = BS.index (unChkExtra ce) 1
