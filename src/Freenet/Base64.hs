
-- | Freenet's modified Base64 encoding
module Freenet.Base64 (
   fromBase64', toBase64'
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BLB
import Data.ByteString.Base64
import qualified Data.Text as T
import Data.Word

toStd :: Word8 -> Word8
toStd x
   | x == (toEnum $ fromEnum '~') = toEnum $ fromEnum '+'
   | x == (toEnum $ fromEnum '-') = toEnum $ fromEnum '/'
   | otherwise = x

toStandardAlphabet :: B.ByteString -> B.ByteString
toStandardAlphabet = B.map toStd

fromBase64' :: T.Text -> Either T.Text B.ByteString
fromBase64' s = case dec of
  Left e   -> Left $ T.pack e
  Right bs -> Right bs
  where
    dec = decode $ toStandardAlphabet bs
    bs = BL.toStrict $ BL.concat
         [ BLB.toLazyByteString $ BLB.string8 (T.unpack s)
         , BL.replicate pad $ toEnum $ fromEnum '='
         ]
    pad
      | T.length s `rem` 4 == 0 = 0
      | otherwise = fromIntegral $ 4 - (T.length s `rem` 4)
  
toBase64' :: B.ByteString -> T.Text
toBase64' b = T.pack $ map (toEnum . fromEnum) $ B.unpack $ encode b

