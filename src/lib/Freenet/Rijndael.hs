{-# LANGUAGE ForeignFunctionInterface #-}

module Freenet.Rijndael (
   initRijndael,
   Key, initKey, encipher
   ) where
   
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as HEX
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.IO.Unsafe ( unsafePerformIO )

newtype Key = Key B.ByteString

instance Show Key where
   show (Key k) = show $ HEX.encode k
 
bsToRows :: Int -> CUInt
bsToRows bs
   | bs == 16 = 4
   | bs == 24 = 6
   | bs == 32 = 8
   | otherwise = error "unsupported block size (must be 16, 24 or 32 bytes)"

-- |
-- initializes some Rijndael tables and *must* be run before
-- using any other functions from this module
initRijndael :: IO ()
initRijndael = c_init_tables

-- | initialize key for en- and decryption use
initKey
   :: Int            -- ^ block size (16, 24 or 32 bytes)
   -> B.ByteString   -- ^ key (16, 24 or 32 bytes)
   -> Key            -- ^ the resulting key schedule
{-# NOINLINE initKey #-}
initKey bs b
   | len == 16 = doInit 4
   | len == 24 = doInit 6
   | len == 32 = doInit 8
   | otherwise = error "initKey: wrong key size (must be 16, 24 or 32 bytes)"
   where
      len = B.length b :: Int
      doInit nk = unsafePerformIO $ unsafeUseAsCString b $ \ikey -> do
         keySize <- c_rijndael_sched_key_size
         ptr <- mallocBytes keySize
         c_rijn_sched_key ptr (castPtr ikey) (bsToRows bs) nk
         fptr <- newForeignPtr c_free_finalizer (castPtr ptr)
         return $ Key $ fromForeignPtr fptr 0 keySize

encipher :: Key -> B.ByteString -> B.ByteString
{-# NOINLINE encipher #-}
encipher (Key k) b
   | B.length b /= 32 = error "encipher: wrong block size (must be 32 bytes)"
   | otherwise = unsafePerformIO $ create 32 $ \rp -> do
      let (kp, off, _) = toForeignPtr k in withForeignPtr kp $ \kpf ->
         let (ip, ioff, _) = toForeignPtr b in withForeignPtr ip $ \ipf ->
            c_rijn_encrypt (castPtr $ kpf `plusPtr` off) (castPtr $ ipf `plusPtr` ioff) (castPtr rp)

foreign import ccall "rijndael.h init_tables"
   c_init_tables :: IO ()
   
foreign import ccall "rijndael.h rijndael_sched_key_size"
   c_rijndael_sched_key_size :: IO Int
   
foreign import ccall "rijndael.h rijndael_init_key"
   c_rijn_sched_key :: Ptr Key -> CString -> CUInt -> CUInt -> IO ()
   
foreign import ccall "rijndael.h encrypt_block"
   c_rijn_encrypt :: Ptr Key -> CString -> CString -> IO ()
   
