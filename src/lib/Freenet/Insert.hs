
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Insert (
  InsertTarget(..), InsertData(..), insert
  ) where

import Control.Concurrent ( forkIO )
import Control.Monad ( void )
import qualified Data.Binary as BIN
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA
import qualified Data.Text as T

import Freenet.Chk
import Freenet.Metadata
import Freenet.Types
import Freenet.URI
import Requests
import Utils

data InsertTarget
  = InsertCHK      -- ^ CHK insert

data InsertData
  = InsertDirect
    { insertDirectData :: BSL.ByteString
    , insertDirectMime :: T.Text
    }

data InsertProgress = IP
                      { chkTodo :: ! [ChkBlock]
                      , _ipUri  :: URI
                      } deriving ( Show )

-- |
-- Prepares some data for insert, either creating a single CHK
-- block if it's 32kiB or less in size, or by creating a splitfile for
-- larger chunks.
packChk
  :: BSL.ByteString              -- ^ the data to insert
  -> ([ChkBlock], BS.ByteString) -- ^ the (CHK blocks, SHA256(data), fetch URI) for the data.
                                 --   the *first* block contains metadata head, if any
packChk d
  | BSL.length d <= chkDataSize = let blk = encryptChk (bsToStrict d) key in ([blk], unKey key)
  | otherwise = error "can't pack splitfiles yet"
  where
    key = mkKey' $ bsToStrict $ bytestringDigest $ sha256 d

insert :: a -> InsertTarget -> InsertData -> IO InsertProgress
insert _ InsertCHK (InsertDirect payload mime) = do
  let
    (blks, hash)  = packChk payload
    duri          = chkBlockUri (head blks) (mkKey' hash)
    md            = Manifest [("", SimpleRedirect [(SHA256, hash)] (RedirectKey (Just mime) duri))]
    (mdblks, mdh) = packChk $ BIN.encode md
    mduri         = CHK (chkBlockKey $ head mdblks) (mkKey' mdh) (mkChkExtra 3 (-1) True) []
    ip            = IP (blks ++ mdblks) mduri
    
--  void $ forkIO $ mapM_ (insertChk ins) (chkTodo ip)
  error "Insert.insert is not implemented"
  return ip
  
