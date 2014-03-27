
{-# LANGUAGE OverloadedStrings #-}

-- |
-- High level data fetching. This includes splitfile reassembly,
-- handling of archives and retries.
module Freenet.Fetch (
  fetchUri
  ) where

import qualified Codec.Archive.Tar as TAR
import qualified Data.ByteString.Lazy as BSL
import Data.Either ( partitionEithers )
import qualified Data.Map.Strict as Map
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import System.Log.Logger

import Freenet
import Freenet.Compression
import Freenet.Metadata
import Freenet.URI

logI :: String -> IO ()
logI m = infoM "freenet.fetch" m

-- |
-- Tries to fetch the specified URI, parses metadata if it's
-- a control document, goes on fetching the referenced data,
-- and finally returns everything
fetchUri :: Freenet -> URI -> IO (Either T.Text BSL.ByteString)
fetchUri fn uri = do
  logI $ "fetching " ++ show uri
  
  db <- requestData fn uri
  
  case db of
    Left e  -> return $ Left e
    Right plaintext -> if isControlDocument uri
                       then do
                         case parseMetadata plaintext of
                           Left e   -> return $ Left e
                           Right md -> resolvePath fn (uriPath uri) Nothing (Just md)
                       else return $ Right plaintext

fetchUris :: Freenet -> [URI] -> IO [(URI, Either T.Text BSL.ByteString)]
fetchUris fn uris = do
  result <- sequence $ map (requestData fn) uris
  return $ zip uris result

fetchRedirect :: Freenet -> RedirectTarget -> [T.Text] -> IO (Either T.Text BSL.ByteString)
fetchRedirect fn (RedirectKey _ uri) path = fetchUri fn $ appendUriPath uri path
--                                            if isControlDocument uri
--                                            then fetchUri fn $ appendUriPath uri path
--                                            else requestData fn uri
fetchRedirect fn (SplitFile comp dlen olen segs _) _ = do -- TODO: we're not returning the MIME
  let
    -- TODO: use FEC check blocks as well
    segUris = catMaybes $ map (\(SplitFileSegment uri isd) -> if isd then Just uri else Nothing) segs

  ds <- fetchUris fn segUris
  
  -- see if everything could be fetched and try to decrypt
  let
    (es, bs) = partitionEithers (map snd ds)

  if (not . null) es
    then return $ Left $ T.intercalate ", " es
    else decompress comp $ BSL.take (fromIntegral dlen) $ BSL.concat bs

type Archive = Map.Map String BSL.ByteString

fetchArchive :: Freenet -> RedirectTarget -> ArchiveManifestType -> IO (Either T.Text Archive)
fetchArchive fn tgt tp = do
  logI $ "fetching archive"
  arch <- fetchRedirect fn tgt []
  case arch of
    Left e   -> return $ Left e
    Right bs -> case tp of
      TAR -> return $ Right $ TAR.foldEntries
             (\e m -> case TAR.entryContent e of
                 TAR.NormalFile ebs _ -> Map.insert (TAR.entryPath e) ebs m
                 _                    -> m
             ) Map.empty (const Map.empty) $ TAR.read bs
      x   -> return $ Left $ T.pack $ "unsupported archive type " ++ show x


      
-- | FIXME: watch out for infinite redirects
resolvePath
  :: Freenet
  -> [T.Text]                          -- ^ path elements to be resolved
  -> Maybe Archive                     -- ^ archive to resolve simple redirects etc against
  -> Maybe Metadata                    -- ^ the metadata where we try to locate the entries in
  -> IO (Either T.Text BSL.ByteString) -- ^ either we fail, or we locate the final redirect step

-- redirect to default entry in manifest, which has a name of ""
resolvePath fn [] a md@(Just (Manifest _)) = do
  logI "redirecting to default entry"
  resolvePath fn [""] a md

-- resolve in archive
resolvePath fn _ (Just amap) (Just (ArchiveInternalRedirect tgt _)) = do
  logI $ "resolving AIR to " ++ show tgt
  case Map.lookup (T.unpack tgt) amap of
    Nothing -> return $ Left $ "could not locate \"" `T.append` tgt `T.append` "\" in archive"
    Just bs -> do
      logI $ "found " ++ (show tgt) ++ " in archive"
      return $ Right bs

-- follow simple redirects
resolvePath fn ps _ (Just (SimpleRedirect _ tgt)) = do
  logI $ "following simple redirect to " ++ (show tgt) ++ " for " ++ show ps
  fetchRedirect fn tgt ps
  
-- resolve path in manifest
resolvePath fn (p:ps) a md@(Just (Manifest me)) = do
  logI $ "resolving " ++ show p ++ " in manifest"
  
  case lookup p me of
    Nothing -> return $ Left $ "could not locate \"" `T.append` p `T.append` "\" in manifest"
    Just me -> case me of
      SymbolicShortlink tgt -> do
        logI $ "following SSL to " ++ show tgt
        resolvePath fn (tgt:ps) a md
      x                     -> resolvePath fn ps    a  (Just x)
                             -- return $ Left $ T.concat ["can not resolve ", T.pack $ show x, " against a manifest"]

resolvePath fn ps Nothing (Just (ArchiveManifest (RedirectKey _ uri) atype _ _)) = do
  logI $ "looking up archive at " ++ (show uri) ++ " for " ++ show ps
--  arch <- fetchArchive fn uri
  fetchUri fn $ appendUriPath uri ps
  
resolvePath fn ps Nothing (Just (ArchiveManifest atgt atype _ _)) = do
  logI $ "resolving archive manifest " ++ show atgt
  
  arch <- fetchArchive fn atgt atype
  case arch of
    Left e -> return $ Left e
    Right emap -> do
      logI $ "archive entries: " ++ show (Map.keys emap)

      case Map.lookup ".metadata" emap of
        Nothing -> return $ Left $ T.pack $ "no .metadata entry found in TAR archive: " ++ show ps
        Just mdbs -> case parseMetadata mdbs of
          Right md -> resolvePath fn ps (Just emap) (Just md) --  set archive as new reference point
          x        -> return $ Left $ "error parsing manifest from TAR archive: " `T.append` (T.pack $ show x)


-- give up resolving this path
resolvePath _ ps a md = return $ Left $ T.concat [
  "cannot locate ", T.pack (show ps), " in ", T.pack (show md), " with archive", T.pack (show a)]


{-

    
-- resolve inside archive 
resolvePath fn (Just aes) [p] _ = case Map.lookup (T.unpack p) aes of
  Nothing -> return $ Left $ "could not find inA" `T.append` p
  Just bs -> return $ Right bs

resolvePath fn _ ps (Just (ArchiveManifest tgt at _ None)) = print ("am", ps) >> do
  arch <- fetchArchive fn tgt at
  case arch of
    Left e -> return $ Left e
    Right emap -> case Map.lookup ".metadata" emap of
      Nothing -> return $ Left $ T.pack $ "no .metadata entry found in TAR archive: " ++ show emap
      Just mdbs -> case parseMetadata mdbs of
        Right md -> resolvePath fn (Just emap) ps (Just md) --  set archive as new reference point
        x        -> return $ Left $ "error parsing manifest from TAR archive: " `T.append` (T.pack $ show x)
      
resolvePath fn _ [] (Just (SimpleRedirect _ tgt)) = print "sr" >> fetchRedirect fn tgt
--resolvePath fn [] (Left (SymbolicShortlink tgt)) = print "ssl" >> resolvePath fn arch [tgt] m

{-
resolvePath fn ps (Right emap) = Left "emap"
resolvePath fn arch@(Just (ae, _)) [] (ArchiveInternalRedirect tgt _) = print ("air " `T.append` tgt) >> case Map.lookup tgt ae of
  Nothing -> print ae >> (return $ Left $ "could not find in archive: " `T.append` tgt)
  Just bs -> return $ Right bs
-}
-}
