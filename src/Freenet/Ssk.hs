
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Freenet.Ssk (
  SskFound(..), mkSskFound, sskLocation', sskEncryptDocname,
  
  -- * Requesting SSKs
  SskRequest(..),
  
  -- * SSK Headers
  SskHeader, mkSskHeader, sskDataSize, sskHeaderSize,
  sskPersist
  ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put ( putByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy ( fromStrict )
import Data.Digest.Pure.SHA
import qualified Data.Text as T
import Data.Text.Encoding ( encodeUtf8 )

import Freenet.Base64
import Freenet.Dsa
import Freenet.Pcfb
import qualified Freenet.Rijndael as RD
import Freenet.Store
import Freenet.Types
--import Freenet.URI

--------------------------------------------------------------------------------
-- SSK data
--------------------------------------------------------------------------------

-- |
-- the header of an SSK data block, consisting of:
--    0  word16     : hash algorithm, must be SHA256
--    2  word16     : symmetric cipher identifier
--    4  32 bytes   : E(H(docname))
--   36  36 bytes   : encrypted part of the headers
--   72  32 bytes   : signature parameter R
--  104  32 bytes   : signature parameter S
--  136             : bytes total length
newtype SskHeader = SskHeader { _unSskHeader :: BS.ByteString } deriving ( Eq )

sskHeaderSize :: Int
sskHeaderSize = 136

-- | size of the SSK payload
sskDataSize :: Int
sskDataSize = 1024

instance Show SskHeader where
  show (SskHeader bs) = T.unpack $ toBase64' bs

instance Binary SskHeader where
  put (SskHeader h) = putByteString h
  get = SskHeader <$> getByteString sskHeaderSize

mkSskHeader :: BS.ByteString -> Either T.Text SskHeader
mkSskHeader bs
  | BS.length bs == sskHeaderSize = Right $ SskHeader bs
  | otherwise = Left $ "SSK header length must be 136 bytes, got " `T.append` T.pack (show $ BS.length bs)

data SskFound = SskFound !Key !SskHeader !BS.ByteString

instance Show SskFound where
  show (SskFound k h d) = "SskFound {k=" ++ show k ++ ", h=" ++ (show h) ++ ", len=" ++ (show $ BS.length d) ++ "}"

sskPersist :: StorePersistable SskRequest SskFound
sskPersist = SP
  { storePut = \(SskFound k h d) -> put k >> put h >> putByteString d
  , storeGet = \(SskRequest pk _ _) -> do
       (k, h, d) <- (,,) <$> get <*> get <*> getByteString sskDataSize
       case mkSskFound k h d pk of
         Right df -> return df
         Left e   -> fail $ T.unpack e
  }
  
instance DataFound SskFound where
  dataFoundLocation (SskFound k _ _) = k

data SskRequest = SskRequest !PubKey !Key !Word8 -- ^ the pubkey, encrypted.hashed docname and algoritms
                deriving ( Show )

instance DataRequest SskRequest where
  dataRequestLocation (SskRequest pk ehd _) = sskLocation (hashPubKey pk) ehd

-- mkSskRequest :: 
-- mkSskRequest (SSK hpk ck e doc _) = SskRequest hpk (sskEncryptDocname ck doc) (sskExtraCrypto e)


mkSskFound
  :: Key                      -- ^ location
  -> SskHeader                -- ^ header
  -> BS.ByteString            -- ^ payload
  -> PubKey                   -- ^ public key needed for verifying the signature
  -> Either T.Text SskFound
mkSskFound k h d pubkey
  | otherwise = Left $ "can't mkSskFound by now..."--  SskFound k h d



-- |
-- for SSKs, the routing key is determined by
-- H(PK) and the encrypted document name's hash E(H(docname))
sskLocation'
  :: Key    -- ^ the public key hash
  -> Key    -- ^ the crypto key (required to encrypt the docname)
  -> T.Text -- ^ the document name
  -> Key    -- ^ the resulting routing key
sskLocation' hpk ckey docname = sskLocation hpk ehd where
  ehd = sskEncryptDocname ckey docname

-- |
-- determines the location for a SSK document
sskLocation
  :: Key    -- ^ hash (public key)
  -> Key    -- ^ encrypt ( hash ( docname ) )
  -> Key    -- ^ routing key
sskLocation hpk ehd = mkKey' $ BSL.toStrict $ bytestringDigest $ sha256 $ BSL.fromChunks [unKey ehd, unKey hpk]

-- |
-- encrypts the hash of an SSK document name. this is needed
-- to determine the location of an SSK document
sskEncryptDocname
  :: Key    -- ^ the crypto key (second part of the SSK URI) 
  -> T.Text -- ^ the document name (first path element of SSK URI)
  -> Key    -- ^ the encrypted document name
sskEncryptDocname ckey docname = mkKey' $ RD.encipher rjk dnh where
  rjk = RD.initKey 32 $ unKey ckey -- prepare encryption key
  dnh = BSL.toStrict $ bytestringDigest $ sha256 $ BSL.fromStrict (encodeUtf8 docname)

