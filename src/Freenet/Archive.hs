
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Archive (
  Archive, fetchArchive
  ) where

import qualified Codec.Archive.Tar as TAR
import qualified Codec.Archive.Zip as ZIP
import Control.Exception ( catch, ErrorCall )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Log.Logger

import Freenet.Metadata
import Freenet.SplitFile
import Node

type Archive = (Map.Map String BSL.ByteString)

logI :: String -> IO ()
logI m = infoM "freenet.archive" m

----------------------------------------------------------------------------------------------------
-- Archives
----------------------------------------------------------------------------------------------------

fetchArchive :: Show a => Node a -> RedirectTarget -> ArchiveType -> IO (Either T.Text Archive)
fetchArchive fn tgt tp = do
  logI $ "fetching archive " ++ show tgt
  
  arch <- fetchRedirect' fn tgt

  let
    parseZip zbs = catch (return $ Right go) handler where
      handler :: ErrorCall -> IO (Either T.Text Archive)
      handler e = return $ Left $ T.pack $ show e
      
      entries = ZIP.zEntries $ ZIP.toArchive zbs
      
      go = Map.fromList $ map (\e -> (ZIP.eRelativePath e, ZIP.fromEntry e)) entries
      
  case arch of
    Left e   -> return $ Left e
    Right bs -> case tp of
      TAR -> return $ Right $ TAR.foldEntries
             (\e m -> case TAR.entryContent e of
                 TAR.NormalFile ebs _ -> Map.insert (TAR.entryPath e) ebs m
                 _                    -> m
             ) Map.empty (const Map.empty) $ TAR.read bs
      ZIP -> parseZip bs
--      x   -> return $ Left $ T.pack $ "unsupported archive type " ++ show x

fetchRedirect' :: Show a => Node a -> RedirectTarget -> IO (Either T.Text BSL.ByteString)
fetchRedirect' fn (RedirectKey _ uri) = do
  logI $ "fetch redirect' to " ++ show uri
  mbs <- requestNodeData fn uri
  case mbs of
    Left e   -> return $ Left $ "fetchRedirect': error with requestNodeData: " `T.append` e
    Right bs -> case parseMetadata bs of
      Left _  -> return $ Right bs  -- return $ Left $ "fetchRedirect': can't parse metadata: " `T.append` e'
      Right md -> case md of
        ArchiveManifest (RedirectSplitFile sf) _ _ _ -> fetchSplitFile fn sf
        _ -> return $ Left $ "fetchRedirect': what shall I do with metadata: " `T.append` (T.pack $ show md)

fetchRedirect' fn (RedirectSplitFile sf) = fetchSplitFile fn sf
