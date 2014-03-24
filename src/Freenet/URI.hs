
{-# LANGUAGE OverloadedStrings #-}

module Freenet.URI (
  URI(..), parseUri, uriLocation,
  isControlDocument, uriPath, uriCryptoKey,

  -- * CHKs
  ChkExtra, mkChkExtra, chkExtraCrypto,

  -- * SSKs
  sskExtraCrypto, sskEncryptDocname
  ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( replicateM )
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSB
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8' )

import Freenet.Base64
import Freenet.Ssk
import Freenet.Types  

data URI
     = CHK
       { chkLocation   :: Key      -- ^ the routing key
       , chkKey        :: Key      -- ^ the crypto key
       , chkExtra      :: ChkExtra -- ^ extra data about algorithms used, always 5 bytes
       , chkPath       :: [T.Text] -- ^ the path, already split at "/" chars
       }
     | SSK
       { sskPubKeyHash :: Key      -- ^ the hash of the public key 
       , sskKey        :: Key      -- ^ the crypto key which allows to decrypt the payload
       , sskExtra      :: SskExtra -- ^ extra information about used algorithms and document type
       , sskDocName    :: T.Text   -- ^ the document name, which is the mandatory first path element
       , sskPath       :: [T.Text] -- ^ the remainder of the path
       }

instance Show URI where
  show (CHK l k e p) = "CHK@" ++ show l ++ "," ++ show k ++ "," ++ show e ++ "/" ++ (T.unpack $ T.intercalate "/" p)
  show (SSK l k e d p) = "SSK@" ++ show l ++ "," ++ show k ++ "," ++ show e ++ "/" ++ T.unpack d ++ "/" ++ (T.unpack $ T.intercalate "/" p)

uriCryptoKey :: URI -> Key
uriCryptoKey (CHK _ k _ _)   = k
uriCryptoKey (SSK _ k _ _ _) = k

-- |
-- this should be compatible with Java's DataOutput.writeUTF(..)
-- method. FIXME: make it so!
getUTF8 :: Get T.Text
getUTF8 = do
  len <- getWord16be
  bs <- getByteString $ fromIntegral len
  case decodeUtf8' bs of
    Left e  -> fail $ "error in getUTF8: " ++ show e
    Right t -> return t
  
instance Binary URI where
  put _ = error "can't put URIs yet"
  get = do
    t <- getWord8
    
    case t of
      1 -> do -- CHK
        fail "get CHK"
        
      2 -> do -- SSK
        rk <- get
        ck <- get
        ex <- get
        dn <- getUTF8
        mc <- getWord32be
        ps <- replicateM (fromIntegral mc) $ getUTF8
        return $ SSK rk ck ex dn ps

      x -> fail $ "unknown URI type " ++ show x
      
parseUri :: T.Text -> Either T.Text URI
parseUri str = case T.take 4 str of
  "CHK@" -> parseChk (T.drop 4 str)
  "SSK@" -> parseSsk (T.drop 4 str)
  _      -> Left $ T.concat ["cannot recognize URI type of \"", str, "\""]

parseSsk :: T.Text -> Either T.Text URI
parseSsk str = let (str', path) = T.span (/= '/') str in case T.split (== ',') str' of
  [rstr, cstr, estr] -> do
    rk <- fromBase64' rstr >>= mkKey
    ck <- fromBase64' cstr >>= mkKey
    e <- fromBase64' estr >>= \eb -> if BS.length eb == 5
                                     then Right $ eb
                                     else Left "CHK extra data must be 5 bytes"
    let
      path' = T.drop 1 path
      ps = if T.null path' then [] else  T.split (== '/') path'

    -- for SSKs, the first path element is the docname and mandatory
    if null ps
      then Left "missing SSK document name"
      else Right $ SSK rk ck (SskExtra e) (head ps) (tail ps)
  _ -> Left $ T.concat $ ["expected 3 comma-separated parts in \"", str, "\""]

parseChk :: T.Text -> Either T.Text URI
parseChk str = let (str', path) = T.span (/= '/') str in case T.split (== ',') str' of
  [rstr, cstr, estr] -> do
    rk <- fromBase64' rstr >>= mkKey
    ck <- fromBase64' cstr >>= mkKey
    e <- fromBase64' estr >>= \eb -> if BS.length eb == 5
                                     then Right $ eb
                                     else Left "CHK extra data must be 5 bytes"
    let
      path' = T.drop 1 path
      ps = if T.null path' then [] else  T.split (== '/') path'
                                          
    return $ CHK rk ck (ChkExtra e) ps -- T.split (== '/') (T.drop 1 path)
  _ -> Left $ T.concat $ ["expected 3 comma-separated parts in \"", str, "\""]

-- |
-- Decides if an URI is expected to point to a metadata block
-- (aka control document). This is usually the case for URIs
-- presented to the user. The control document will generally
-- provide information how to assemble the original data referenced
-- by the URI and specify an MIME type.
isControlDocument :: URI -> Bool
isControlDocument (CHK _ _ e _)   = chkExtraIsControl e
isControlDocument (SSK _ _ _ _ _) = True -- ^ to my knowledge, this is so

-- |
-- Determines the location (aka "routing key") for an URI.
uriLocation :: URI -> Key
uriLocation (CHK loc _ _ _) = loc
uriLocation (SSK hpk ck _ doc _) = sskLocation' hpk ck doc

uriPath :: URI -> [T.Text]
uriPath (CHK _ _ _ p)   = p
uriPath (SSK _ _ _ _ p) = p

--------------------------------------------------------------------------------------
-- CHK extra data (last URI component)
--------------------------------------------------------------------------------------

newtype ChkExtra = ChkExtra { unChkExtra :: BS.ByteString }

instance Show ChkExtra where
  show (ChkExtra bs) = T.unpack $ toBase64' bs

instance Binary ChkExtra where
  put (ChkExtra bs) = putByteString bs
  get = ChkExtra <$> getByteString 5

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

chkExtraIsControl :: ChkExtra -> Bool
chkExtraIsControl ce = 2 == BS.index (unChkExtra ce) 2

---------------------------------------------------------------------------
-- SSK specifics
---------------------------------------------------------------------------

newtype SskExtra = SskExtra { unSskExtra :: BS.ByteString }

instance Show SskExtra where
  show (SskExtra bs) = T.unpack $ toBase64' bs
  
instance Binary SskExtra where
  put (SskExtra bs) = putByteString bs
  get = SskExtra <$> getByteString 5

-- |
-- extracts the crypto algorithm from SSK extra data,
-- which is at (zero based) index 2
sskExtraCrypto :: SskExtra -> Word8
sskExtraCrypto se = BS.index (unSskExtra se) 2
