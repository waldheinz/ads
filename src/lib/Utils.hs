
module Utils (
  bsFromStrict, bsToStrict
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- |
-- Can't use the one from BSL as it's not available with GHC 7.4.
bsFromStrict :: BS.ByteString -> BSL.ByteString
bsFromStrict bs = BSL.fromChunks [bs]

-- |
-- Can't use the one from BSL as it's not available with GHC 7.4.
bsToStrict :: BSL.ByteString -> BS.ByteString
bsToStrict = BS.concat . BSL.toChunks
