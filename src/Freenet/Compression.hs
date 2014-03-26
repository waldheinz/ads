
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Compression (
  CompressionCodec(..), decompress
  ) where

import qualified Codec.Compression.GZip as Gzip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Unsafe ( unsafeUseAsCString )
import qualified Data.ByteString.Lazy as BSL
import Data.Void ( Void )
import Data.Word ( Word8 )
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Text as T
import System.IO.Unsafe

-- |
-- Supported compression algorithms
data CompressionCodec = None | Gzip | Bzip2 | LZMA | LZMA_NEW deriving ( Show )

decompress :: CompressionCodec -> BSL.ByteString -> IO (Either T.Text BSL.ByteString)
decompress comp cdata = case comp of
  None     -> return $ Right $ cdata
  Gzip     -> return $ Right $ Gzip.decompress cdata -- FIXME: does decompress throw on illegal input? seems likely
  LZMA     -> do
    let lzma = initLzma $ BS.pack [0x5d, 0x00, 0x00, 0x10, 0x00]
    return $ Right $ BSL.fromStrict $ decodeLzma lzma $ BSL.toStrict cdata
  LZMA_NEW -> do
--    C.runResourceT ((bsSource d) C.$= (LZMA.decompress Nothing) C.$$ (C.fold (\l c -> BSL.append l (BSL.fromStrict c))) BSL.empty) >>= return . Right
      return $ Left "can't decompress LZMA_NEW :-/"
        
  x        -> return $ Left $ T.pack $ "unsupported compression codec " ++ show x

----------------------------------------------------------------------------------
-- LZMA
----------------------------------------------------------------------------------              

newtype LzmaDec = LzmaDec (ForeignPtr Void)

initLzma :: BS.ByteString -> LzmaDec
{-# NOINLINE initLzma #-}
initLzma props = unsafePerformIO $ unsafeUseAsCString props $ \props' -> do
  dec  <- c_lzma_dec_init props'
  fptr <- newForeignPtr c_lzma_dec_free dec
  return $ LzmaDec fptr

decodeLzma :: LzmaDec -> BS.ByteString -> BS.ByteString
{-# NOINLINE decodeLzma #-}
decodeLzma (LzmaDec fpdec) input = unsafePerformIO $ withForeignPtr fpdec $ \dec -> do
  let
    (ifptr, ioff, ilen) = BSI.toForeignPtr input
  osize <- mallocForeignPtr
  od <- withForeignPtr ifptr $ \iptr ->
    withForeignPtr osize $ \os -> do
      c_lzma_decode dec (iptr `plusPtr` ioff) (fromIntegral ilen) os

  ofptr <- newForeignPtr BSI.c_free_finalizer od
  osize' <- withForeignPtr osize peek
  return $ BSI.fromForeignPtr ofptr 0 (fromIntegral osize')
    
foreign import ccall "lzma.h lzma_dec_init"
  c_lzma_dec_init :: Ptr a -> IO (Ptr Void)

foreign import ccall "lzma.h &lzma_dec_free"
  c_lzma_dec_free :: FunPtr (Ptr Void -> IO ())

-- void* lzma_decode(struct lzma_dec_state *state, void *src, size_t src_len, size_t *dest_len) {
foreign import ccall "lzma.h lzma_decode"
  c_lzma_decode :: Ptr Void
                   -> Ptr Word8 -> CSize
                   -> Ptr CSize
                   -> IO (Ptr Word8)
                   
