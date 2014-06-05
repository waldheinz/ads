
module Properties.CHK ( chkTests ) where

import Data.ByteString as BS
import Data.Word ( Word8 )
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Freenet.Chk
import Freenet.Types

chkTests :: [Test]
chkTests =
  [ testProperty "d(e(x)) == x" dex
  ]

dex :: Key -> [Word8] -> Bool
dex key ds = case decryptChk (encryptChk d key) key of
  Left _          -> False
  Right (d', len) -> d == BS.take len d'
  where
    d = BS.pack ds
