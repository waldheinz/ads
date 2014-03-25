
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Compression (
  CompressionCodec(..), decompress
  ) where

import qualified Codec.Compression.GZip as Gzip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Lzma as LZMA
import qualified Data.Text as T

-- |
-- Supported compression algorithms
data CompressionCodec = None | Gzip | Bzip2 | LZMA | LZMA_NEW deriving ( Show )

decompress :: CompressionCodec -> BSL.ByteString -> IO (Either T.Text BSL.ByteString)
decompress comp cdata = case comp of
  None     -> return $ Right $ cdata
  Gzip     -> return $ Right $ Gzip.decompress cdata -- FIXME: does decompress throw on illegal input? seems likely
  LZMA_NEW -> do
    BSL.writeFile "asdfasdf" cdata
    C.runResourceT ((bsSource d) C.$= (LZMA.decompress Nothing) C.$$ (C.fold (\l c -> BSL.append l (BSL.fromStrict c))) BSL.empty) >>= return . Right
      
      where
        d = (BSL.fromChunks [BS.singleton 0xFD, BSC.pack "7zXZ\0"]) `BSL.append` (BSL.drop 0 cdata)
        
  x        -> return $ Left $ T.pack $ "unsupported compression codec " ++ show x
  
bsSource :: Monad m => BSL.ByteString -> C.Source m BS.ByteString
bsSource bs = C.sourceList $ BSL.toChunks bs

