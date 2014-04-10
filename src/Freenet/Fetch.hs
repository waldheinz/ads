
{-# LANGUAGE OverloadedStrings #-}

-- |
-- High level data fetching. This includes splitfile reassembly,
-- handling of archives and retries.
module Freenet.Fetch (
  fetchUri
  ) where

import Control.Concurrent ( myThreadId )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Log.Logger

import Node
import Freenet.Archive
import Freenet.Metadata
import Freenet.SplitFile
import Freenet.URI

logI :: String -> IO ()
logI m = infoM "freenet.fetch" m
    
-- |
-- Tries to fetch the specified URI, parses metadata if it's
-- a control document, goes on fetching the referenced data,
-- and finally returns everything
fetchUri :: (Show a) => Node a -> URI -> IO (Either T.Text BSL.ByteString)
fetchUri fn uri = do
  tid <- myThreadId
  logI $ "fetching " ++ show uri ++ " on " ++ show tid
  
  db <- requestNodeData fn uri
  
  case db of
    Left e  -> return $ Left e
    Right (pt, ptl) -> let pt' = BSL.take (fromIntegral ptl) $ BSL.fromStrict pt
                       in if isControlDocument uri
                          then case parseMetadata pt' of
                            Left e   -> return $ Left e
                            Right md -> resolvePath fn (uriPath uri) md Nothing
                          else return $ Right pt'

fetchRedirect :: Show a => Node a -> RedirectTarget -> [T.Text] -> IO (Either T.Text BSL.ByteString)
fetchRedirect fn (RedirectKey _ uri) path = do
  logI $ "fetch redirect to " ++ show uri ++ " with path " ++ show path
  fetchUri fn $ appendUriPath uri path
fetchRedirect fn (RedirectSplitFile sf) _ = fetchSplitFile fn sf
    
-- | FIXME: watch out for infinite redirects
resolvePath
  :: Show a => Node a
  -> [T.Text]                          -- ^ path elements to be resolved
  -> Metadata                    -- ^ the metadata where we try to locate the entries in
  -> Maybe Archive                     -- ^ archive to resolve AIR etc. against
  -> IO (Either T.Text BSL.ByteString) -- ^ either we fail, or we locate the final redirect step

-- redirect to default entry in manifest, which has a name of ""
resolvePath fn [] md@(Manifest _) arch = logI "redirecting to default entry" >> resolvePath fn [""] md arch

-- resolve in archive
resolvePath _ _ (ArchiveInternalRedirect tgt _) (Just amap) = do
  logI $ "resolving AIR to " ++ show tgt
  case Map.lookup (T.unpack tgt) amap of
    Nothing -> return $ Left $ "could not locate \"" `T.append` tgt `T.append` "\" in archive"
    Just bs -> do
      logI $ "found " ++ (show tgt) ++ " in archive"
      return $ Right bs

resolvePath fn ps (ArchiveMetadataRedirect tgt) (Just archive) = do
  case Map.lookup (T.unpack tgt) archive of
    Nothing -> return $ Left $ "could not locate metadata " `T.append` tgt `T.append` " in archive"
    Just bs -> case parseMetadata bs of
      Left e   -> return $ Left $ "error parsing metadata from archive: " `T.append` e
      Right md -> resolvePath fn ps md (Just archive)

-- follow simple redirects
resolvePath fn ps (SimpleRedirect _ tgt) _ = do
  logI $ "following simple redirect to " ++ (show tgt) ++ " for " ++ show ps
  fetchRedirect fn tgt ps
  
-- resolve path in manifest
resolvePath fn (p:ps) md@(Manifest me) arch = do
  logI $ "resolving " ++ show p ++ " in manifest"
  
  case lookup p me of
    Nothing -> return $ Left $ "could not locate \"" `T.append` p `T.append` "\" in manifest"
    Just me' -> case me' of
      SymbolicShortlink tgt -> do
        logI $ "following SSL to " ++ show tgt
        resolvePath fn (tgt:ps) md arch
      x                     -> resolvePath fn ps x arch
                             -- return $ Left $ T.concat ["can not resolve ", T.pack $ show x, " against a manifest"]

resolvePath fn ps (ArchiveManifest atgt atype _ _) _ = do
  logI $ "resolving archive manifest " ++ show atgt
  
  arch <- fetchArchive fn (nodeArchives fn) atgt atype
  
  case arch of
    Left e -> return $ Left e
    Right emap -> do
      logI $ "archive entries: " ++ show (Map.keys emap)

      case Map.lookup ".metadata" emap of
        Nothing -> return $ Left $ T.pack $ "no .metadata entry found in TAR archive: " ++ show ps
        Just mdbs -> case parseMetadata mdbs of
          Right md -> resolvePath fn ps md (Just emap)
          x        -> return $ Left $ "error parsing manifest from TAR archive: " `T.append` (T.pack $ show x)

-- give up resolving this path
resolvePath _ ps md arch = return $ Left $ T.concat [
  "cannot locate ", T.pack (show ps), " in ", T.pack (show md), " with archive ", T.pack $ show arch]

