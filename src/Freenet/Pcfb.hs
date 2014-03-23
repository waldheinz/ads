
module Freenet.Pcfb (
   
   -- * PCFB (Periodic Cipher Feed Back) Mode   
   mkPCFB, pcfbEncipher, pcfbEncipherWord8,
   pcfbDecipher, pcfbDecipherWord8
   ) where

import Control.Applicative ( (<*>), (<$>) )
import Control.Monad ( when )
import Control.Monad.ST.Safe
import Data.Bits ( xor )
import qualified Data.ByteString as B
import Data.STRef
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Word

import qualified Freenet.Rijndael as RD
-- import Utils

--------------------------------------------------------------------------------
-- PCFB (Periodic Cipher Feed Back) mode
--------------------------------------------------------------------------------

data PCFB s = MkPCFB
   { _pcfbCipher     :: RD.Key
   , _pcfbFeedback   :: UMV.MVector s Word8
   , _pcfbIdx        :: STRef s Int
   }
   
mkPCFB
   :: RD.Key         -- ^ the underlying cipher to use
   -> B.ByteString   -- ^ the IV (initialization vector)
   -> ST s (PCFB s)
mkPCFB c iv
   | B.length iv /= 32 = error "mkPCFB: IV length must be 32"
   | otherwise = MkPCFB c <$> UV.thaw (bsToVec iv) <*> newSTRef 32

pcfbRefill :: PCFB s -> ST s ()
pcfbRefill (MkPCFB c f i) = do
   pos <- readSTRef i
   when (UMV.length f == pos) $ do
      ff <- UV.freeze f
      fm' <- UV.thaw $ bsToVec $ RD.encipher c $ vecToBs ff
      UMV.copy f fm'
      writeSTRef i 0
      
pcfbDecipherWord8 :: PCFB s -> Word8 -> ST s Word8
pcfbDecipherWord8 pc@(MkPCFB _ f i) x = do
   pcfbRefill pc
   fpos <- readSTRef i
   ff <- UMV.read f fpos
   UMV.write f fpos x
   modifySTRef i (+1)
   return $! x `xor` ff

pcfbDecipher :: PCFB s -> B.ByteString -> ST s B.ByteString
pcfbDecipher pcfb b = B.pack <$> mapM (pcfbDecipherWord8 pcfb) (B.unpack b)

pcfbEncipherWord8 :: PCFB s -> Word8 -> ST s Word8
pcfbEncipherWord8 pc@(MkPCFB _ f i) x = do
   pcfbRefill pc
   fpos <- readSTRef i
   ff <- UMV.read f fpos
   modifySTRef i (+1)
   let result = x `xor` ff
   UMV.write f fpos result
   return $! result

pcfbEncipher :: PCFB s -> B.ByteString -> ST s B.ByteString
pcfbEncipher pcfb b = B.pack <$> mapM (pcfbEncipherWord8 pcfb) (B.unpack b)

-- | converts a ByteString to an unboxed vector
bsToVec :: B.ByteString -> UV.Vector Word8
bsToVec bs = UV.generate (B.length bs) (B.index bs)

vecToBs :: UV.Vector Word8 -> B.ByteString
vecToBs v = B.pack $ UV.toList v
