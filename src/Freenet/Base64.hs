
-- | Freenet's modified Base64 encoding
module Freenet.Base64 (
  FreenetBase64(..),

  -- * Integer utilities
  i2bs, bs2i, bsToPosI
) where

import Data.Bits ( shiftL, shiftR )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BLB
import Data.ByteString.Base64
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Word

-- |
-- things that have a special base64 representation in Freenet
class FreenetBase64 a where
  fromBase64' :: T.Text -> Either T.Text a
  toBase64'   :: a -> T.Text

toStd :: Word8 -> Word8
toStd x
   | x == (toEnum $ fromEnum '~') = toEnum $ fromEnum '+'
   | x == (toEnum $ fromEnum '-') = toEnum $ fromEnum '/'
   | otherwise = x

fromStd :: Char -> Char
fromStd x
   | x == '+' = '~'
   | x == '/' = '-'
   | otherwise = x

toStandardAlphabet :: B.ByteString -> B.ByteString
toStandardAlphabet = B.map toStd

fromBase64Bs :: T.Text -> Either T.Text B.ByteString
fromBase64Bs s = case dec of
  Left e   -> Left $ T.pack e
  Right b -> Right b
  where
    dec = decode $ toStandardAlphabet bs
    bs = BL.toStrict $ BL.concat
         [ BLB.toLazyByteString $ BLB.string8 (T.unpack s)
         , BL.replicate pad $ toEnum $ fromEnum '='
         ]
    pad
      | T.length s `rem` 4 == 0 = 0
      | otherwise = fromIntegral $ 4 - (T.length s `rem` 4)

-- | convert bytes to Freenet's modified base64
toBase64Bs :: B.ByteString -> T.Text
toBase64Bs b = T.takeWhile (/= '=') $
  T.map fromStd $ T.pack $ map (toEnum . fromEnum) $ B.unpack $ encode b

instance FreenetBase64 B.ByteString where
  fromBase64' = fromBase64Bs
  toBase64'   = toBase64Bs

------------------------------------------------------------------------
-- base64 encoded integers
------------------------------------------------------------------------

bs2i :: B.ByteString -> Integer
bs2i b
   | B.length b' == 0 = 0
   | sign = go b' - 2 ^ (B.length b' * 8)
   | otherwise = go b'
   where
      b' = b -- B.dropWhile (==0) b -- drop leading 0s
      sign = B.index b' 0 > 127
      go = B.foldl' (\i bb -> (i `shiftL` 8) + fromIntegral bb) 0 

bsToPosI :: B.ByteString -> Integer
bsToPosI = B.foldl' (\i bb -> (i `shiftL` 8) + fromIntegral bb) 0 

i2bs :: Integer -> B.ByteString
i2bs x
   | x == 0 = B.singleton 0
   | x < 0 = (\xx -> B.reverse $ B.unfoldr go xx) $ 2 ^ (8 * bytes) + x
   | otherwise = if B.index posRes 0 > 127 then B.singleton 0 <> posRes else posRes
   where
      posRes = B.reverse $ B.unfoldr go x
      bytes = (integerLogBase 2 (abs x) + 1) `quot` 8 + 1
      go i = if i == 0 then Nothing
                       else Just (fromIntegral i, i `shiftR` 8)

-- Compute the (floor of the) log of i in base b.
-- Simplest way would be just divide i by b until it's smaller then b,
-- but that would be very slow!  We are just slightly more clever.
integerLogBase :: Integer -> Integer -> Int
integerLogBase b i =
     if i < b then
        0
     else
        -- Try squaring the base first to cut down the number of divisions.
        let l = 2 * integerLogBase (b*b) i
            doDiv :: Integer -> Int -> Int
            doDiv ii ll = if ii < b then ll else doDiv (ii `div` b) (ll+1)
        in  doDiv (i `div` (b^l)) l

instance FreenetBase64 Integer where
  fromBase64' s = fromBase64' s >>= \bs -> Right (bs2i bs)
  toBase64'     = toBase64' . i2bs
