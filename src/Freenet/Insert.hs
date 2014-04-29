
module Freenet.Insert (
  InsertTarget(..), InsertData(..), insert
  ) where

import Control.Concurrent ( forkIO )
import Control.Monad ( void )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Digest.Pure.SHA
import qualified Data.Text as T

import Freenet.Chk
import Freenet.Types
import Freenet.URI
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
                      , ipUri   :: URI
                      } deriving ( Show )

insert :: ChkInsert a => a -> InsertTarget -> InsertData -> IO InsertProgress
insert ins InsertCHK (InsertDirect payload mime) = do
  let
    key = mkKey' $ bsToStrict $ bytestringDigest $ sha256 $ payload
    blk = encryptChk (bsToStrict payload) key
    uri = chkBlockUri blk key
    ip  = IP [blk] uri
  
  void $ forkIO $ mapM_ (insertChk ins) (chkTodo ip)
  
  return ip
