
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Compression (
  CompressionCodec(..), decompress
  ) where

import qualified Codec.Compression.BZip as Bzip
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Exception as CE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BSL
import Data.Void ( Void )
import Data.Word ( Word8 )
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Text as T

-- |
-- Supported compression algorithms
data CompressionCodec = None | Gzip | Bzip2 | LZMA | LZMA_NEW deriving ( Eq, Ord, Show )

decompress :: CompressionCodec -> BSL.ByteString -> IO (Either T.Text BSL.ByteString)
decompress comp cdata = case comp of
  None     -> return $ Right $ cdata
  Bzip2    -> CE.catch (return $ Right $ Bzip.decompress $ BSL.pack [0x42, 0x5a] `BSL.append` cdata) ehandler
  Gzip     -> CE.catch (return $ Right $ Gzip.decompress cdata) ehandler
  LZMA     -> do
    lzma <- initLzma $ BS.pack [0x5d, 0x00, 0x00, 0x10, 0x00]
    dec <- decodeLzma lzma $ toStrict cdata
    return $ Right $ fromStrict dec
  LZMA_NEW -> do
    let (hdr, cd) = BSL.splitAt 5 cdata
    lzma <- initLzma $ toStrict hdr
    dec <- decodeLzma lzma $ toStrict cd
    return $ Right $ fromStrict dec

fromStrict :: BS.ByteString -> BSL.ByteString
fromStrict bs = BSL.fromChunks [bs]

toStrict :: BSL.ByteString -> BS.ByteString
toStrict = BS.concat . BSL.toChunks


ehandler :: CE.ErrorCall -> IO (Either T.Text BSL.ByteString)
ehandler e = return $ Left $ "decompression failed: " `T.append` (T.pack $ show e)

----------------------------------------------------------------------------------
-- LZMA
----------------------------------------------------------------------------------              

newtype LzmaDec = LzmaDec (ForeignPtr Void)

initLzma :: BS.ByteString -> IO LzmaDec
{-# NOINLINE initLzma #-}
initLzma props
  | BS.length props /= 5 = error "props must be 5 bytes"
  | otherwise = do
    let (pfptr, poff, _) = BSI.toForeignPtr props
    dec  <- withForeignPtr pfptr $ \pptr -> do
      c_lzma_dec_init (pptr `plusPtr` poff)
    fptr <- newForeignPtr c_lzma_dec_free dec
    return $ LzmaDec fptr

decodeLzma :: LzmaDec -> BS.ByteString -> IO BS.ByteString
{-# NOINLINE decodeLzma #-}
decodeLzma (LzmaDec fpdec) input = withForeignPtr fpdec $ \dec -> do
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
                   
