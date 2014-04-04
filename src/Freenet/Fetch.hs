
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

import Node
import Freenet.Chk
import Freenet.Compression
import Freenet.Metadata
import Freenet.Ssk
import Freenet.Types
import Freenet.URI

logI :: String -> IO ()
logI m = infoM "freenet.fetch" m

requestNodeData :: (Show a) => Node a -> URI -> IO (Either T.Text BSL.ByteString)
requestNodeData n (CHK loc key extra _) = do
  case chkExtraCompression extra of
    Left  e -> return $ Left $ "can't decompress CHK: " `T.append` e
    Right c -> do
      result <- requestChk n $ ChkRequest loc (chkExtraCrypto extra)
  
      case result of
        Left e    -> return $ Left $ "obtaining CHK data block failed: " `T.append` e
        Right blk -> case decryptDataBlock blk key $ chkExtraCrypto extra of
          Left e  -> return $ Left $ "decrypting CHK data block failed: " `T.append` e
          Right p -> decompressChk c p

requestNodeData n (SSK pkh key extra dn _) = do
  result <- requestSsk n $ SskRequest pkh (sskEncryptDocname key dn) (sskExtraCrypto extra)

  return $ case result of
    Left e    -> Left e
    Right blk -> decryptDataBlock blk key $ sskExtraCrypto extra

requestNodeData n (USK pkh key extra dn dr _) = do
  result <- let dn' = dn `T.append` "-" `T.append` (T.pack $ show dr)
            in requestSsk n $ SskRequest pkh (sskEncryptDocname key dn') (sskExtraCrypto extra)

  return $ case result of
    Left e    -> Left e
    Right blk -> decryptDataBlock blk key $ sskExtraCrypto extra
    
-- |
-- Tries to fetch the specified URI, parses metadata if it's
-- a control document, goes on fetching the referenced data,
-- and finally returns everything
fetchUri :: (Show a) => Node a -> URI -> IO (Either T.Text BSL.ByteString)
fetchUri fn uri = do
  logI $ "fetching " ++ show uri
  
  db <- requestNodeData fn uri
  
  case db of
    Left e  -> return $ Left e
    Right plaintext -> if isControlDocument uri
                       then do
                         print "fetchUri: metadata"
                         case parseMetadata plaintext of
                           Left e   -> return $ Left e
                           Right md -> resolvePath fn (uriPath uri) md Nothing
                       else print "fetchUri: direct" >> (return $ Right plaintext)



fetchUris :: Show a => Node a -> [URI] -> IO [(URI, Either T.Text BSL.ByteString)]
fetchUris fn uris = do
  result <- sequence $ map (requestNodeData fn) uris
  return $ zip uris result

fetchRedirect :: Show a => Node a -> RedirectTarget -> [T.Text] -> IO (Either T.Text BSL.ByteString)
fetchRedirect fn (RedirectKey _ uri) path = do
  logI $ "fetch redirect to " ++ show uri ++ " with path " ++ show path
--   requestNodeData fn uri
  fetchUri fn $ appendUriPath uri path
--  if isControlDocument uri
--  then fetchUri fn $ appendUriPath uri path
--  else requestNodeData fn uri
  
fetchRedirect fn (SplitFile comp dlen olen segs _) _ = do -- TODO: we're not returning the MIME
  logI $ "fetch split file"
  
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

fetchRedirect' :: Show a => Node a -> RedirectTarget -> IO (Either T.Text BSL.ByteString)
fetchRedirect' fn (RedirectKey _ uri) = do
  logI $ "fetch redirect' to " ++ show uri
  mbs <- requestNodeData fn uri
  case mbs of
    Left e   -> return $ Left $ "fetchRedirect': error with requestNodeData: " `T.append` e
    Right bs -> case parseMetadata bs of
      Left e'  -> return $ Right bs  -- return $ Left $ "fetchRedirect': can't parse metadata: " `T.append` e'
      Right md -> case md of
        ArchiveManifest sf@(SplitFile{}) atype _ _ -> fetchRedirect fn sf []
        _ -> return $ Left $ "fetchRedirect': what shall I do with metadata: " `T.append` (T.pack $ show md)

fetchRedirect' fn sf@(SplitFile _ _ _ _ _) = fetchRedirect fn sf []

type Archive = (Map.Map String BSL.ByteString)

fetchArchive :: Show a => Node a -> RedirectTarget -> ArchiveManifestType -> IO (Either T.Text Archive)
fetchArchive fn tgt tp = do
  logI $ "fetching archive " ++ show tgt
  
  arch <- fetchRedirect' fn tgt
  
  case arch of
    Left e   -> return $ Left e
    Right bs -> case tp of
      TAR -> return $ Right $ TAR.foldEntries
             (\e m -> case TAR.entryContent e of
                 TAR.NormalFile ebs _ -> Map.insert (TAR.entryPath e) ebs m
                 _                    -> m
             ) Map.empty (const Map.empty) $ TAR.read bs
      x   -> return $ Left $ T.pack $ "unsupported archive type " ++ show x
{-
resolveInArchive
  :: Show a
  => Node a
  -> Archive
  -> Metadata
  -> [T.Text]
  -> IO (Either T.Text BSL.ByteString)
resolveInArchive _ arch md ps = return $ Left $ T.pack $ "resolveInArchvie: " ++ show ps ++ "   " ++ show arch ++ "(md=
  -}
    
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
resolvePath fn _ (ArchiveInternalRedirect tgt _) (Just amap) = do
  logI $ "resolving AIR to " ++ show tgt
  case Map.lookup (T.unpack tgt) amap of
    Nothing -> return $ Left $ "could not locate \"" `T.append` tgt `T.append` "\" in archive"
    Just bs -> do
      logI $ "found " ++ (show tgt) ++ " in archive"
      return $ Right bs

-- follow simple redirects
resolvePath fn ps (SimpleRedirect _ tgt) _ = do
  logI $ "following simple redirect to " ++ (show tgt) ++ " for " ++ show ps
  fetchRedirect fn tgt ps
  
-- resolve path in manifest
resolvePath fn (p:ps) md@(Manifest me) arch = do
  logI $ "resolving " ++ show p ++ " in manifest"
  
  case lookup p me of
    Nothing -> return $ Left $ "could not locate \"" `T.append` p `T.append` "\" in manifest"
    Just me -> case me of
      SymbolicShortlink tgt -> do
        logI $ "following SSL to " ++ show tgt
        resolvePath fn (tgt:ps) md arch
      x                     -> resolvePath fn ps x arch
                             -- return $ Left $ T.concat ["can not resolve ", T.pack $ show x, " against a manifest"]
{-
resolvePath fn ps ((ArchiveManifest tgt@(RedirectKey _ uri) atype _ _)) = do
  logI $ "looking up archive at " ++ (show uri) ++ " for " ++ show ps
  arch <- fetchArchive fn tgt atype
  case arch of
    Left e -> return $ Left $ "resolvePath (archive manifest): " `T.append` e
    Right a -> resolveInArchive fn a ps
  -}
resolvePath fn ps (ArchiveManifest atgt atype _ _) _ = do
  logI $ "resolving archive manifest " ++ show atgt
  
  arch <- fetchArchive fn atgt atype
  
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
