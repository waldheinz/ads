
{-# LANGUAGE MultiParamTypeClasses #-}

module Freenet.Dsa (
  PubKey, hashPubKey, pubKeySize, dsaStorePersist,

  -- * requesting pubkeys
  PubKeyRequest(..)
  ) where

import qualified Crypto.PubKey.DSA as DSA
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA

import Freenet.Base64
import Freenet.Store
import Freenet.Types

putMpi :: Integer -> Put
putMpi i = putWord16be (fromIntegral $ BS.length bs) >> putByteString bs where
  bs = i2bs i

putGroup :: DSA.Params -> Put
putGroup (DSA.Params p g q) = putMpi p >> putMpi q >> putMpi g

newtype PubKey = PK { _unPublicKey :: DSA.PublicKey } deriving ( Show )

instance DataFound PubKey where
  dataFoundLocation = hashPubKey

pubKeySize :: Int
pubKeySize = 1024

-- |
-- put without padding
putPublicKey :: PubKey -> Put
putPublicKey (PK (DSA.PublicKey grp y)) = putGroup grp >> putMpi y

hashPubKey :: PubKey -> Key
hashPubKey pk = mkKey' $ BSL.toStrict $ bytestringDigest $ sha256 $ runPut $ putPublicKey pk

-- |
-- a pubkey is adressed soly by it's hash
newtype PubKeyRequest = PubKeyRequest { unPubKeyRequest :: Key } deriving ( Show )

instance DataRequest PubKeyRequest where
  dataRequestLocation = unPubKeyRequest

putPk :: PubKey -> Put
putPk pk = putLazyByteString d >> putLazyByteString pad where
  pad = BSL.replicate (fromIntegral $ pubKeySize - (fromIntegral $ BSL.length d)) 0
  d   = runPut $ putPublicKey pk

dsaStorePersist :: StorePersistable PubKeyRequest PubKey
dsaStorePersist =
  SP
    { storeSize = pubKeySize
    , storePut  = putPk
    , storeGet = \_ -> fail $ "can't get pubkeys"
    }

