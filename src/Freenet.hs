
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, initFn, fetchUri
  ) where

import qualified Codec.Archive.Tar as TAR
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void, when )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import Data.Either ( partitionEithers )
import qualified Data.Map.Strict as Map
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import System.FilePath ( (</>) )
import System.Log.Logger

import Freenet.Chk
import Freenet.Compression
import qualified Freenet.Companion as FC
import Freenet.Keys
import Freenet.Metadata
import Freenet.Ssk
import qualified Freenet.Store as FS
import Freenet.Types
import Freenet.URI

import Debug.Trace

data Freenet = FN
               { fnChkStore    :: FS.StoreFile ChkFound
               , fnCompanion   :: Maybe FC.Companion
               , fnIncomingChk :: TChan ChkFound
               , fnIncomingSsk :: TChan SskFound
               }

logI :: String -> IO ()
logI m = infoM "freenet" m

-- | initializes Freenet subsystem
initFn :: CFG.Config -> IO Freenet
initFn cfg = do
  -- datastore
  dsdir       <- CFG.require cfg "datastore"
  chkStore    <- FS.mkStoreFile (undefined :: ChkFound) (dsdir </> "store-chk") (1024 * 16)
  
  chkIncoming <- newBroadcastTChanIO
  sskIncoming <- newBroadcastTChanIO
  
  let fn = FN chkStore Nothing chkIncoming sskIncoming

  -- companion
  let ccfg = CFG.subconfig "companion" cfg
  chost <- CFG.lookup ccfg "host" :: IO (Maybe String)
  case chost of
    Nothing -> return fn
    Just _  -> do
      comp <- FC.initCompanion ccfg (offerChk fn True) (offerSsk fn True)
      return $ fn { fnCompanion = Just comp }

offerSsk :: Freenet -> Bool -> SskFound -> STM ()
offerSsk fn _ df = do
  -- write to our store
--  when toStore $ FS.putData (fnSskStore fn) df
  
  -- broadcast arrival
  writeTChan (fnIncomingSsk fn) df
  

offerChk :: Freenet -> Bool -> ChkFound -> STM ()
offerChk fn toStore df = do
  -- write to our store
  when toStore $ FS.putData (fnChkStore fn) df
  
  -- broadcast arrival
  writeTChan (fnIncomingChk fn) df

handleDataRequest :: Freenet -> DataRequest -> IO ()
handleDataRequest fn dr = do
  fromStore <- case dr of
    (ChkRequest { }) -> FS.getData (fnChkStore fn) dr
    _ -> return Nothing
  
  case fromStore of
    Just df -> atomically $ offerChk fn False df
    Nothing -> case fnCompanion fn of
      Nothing -> return ()
      Just c  -> FC.get c dr

fetchUris :: Freenet -> [URI] -> IO [(URI, Either T.Text BSL.ByteString)]
fetchUris fn uris = do
  result <- sequence $ map (fetchUri fn) uris
  return $ zip uris result

fetchRedirect :: Freenet -> RedirectTarget -> IO (Either T.Text BSL.ByteString)
fetchRedirect fn (RedirectKey _ uri) = fetchUri fn uri
fetchRedirect fn (SplitFile comp dlen olen segs _) = do -- TODO: we're not returning the MIME
  let
    -- TODO: use FEC check blocks as well
    segUris = catMaybes $ map (\(SplitFileSegment uri isd) -> if isd then Just uri else Nothing) segs

  ds <- fetchUris fn segUris
  
  -- see if everything could be fetched and try to decrypt
  let
    (es, bs) = partitionEithers (map snd ds)

  if (not . null) es
    then return $ Left $ T.intercalate ", " es
    else do
      let cdata = BSL.take (fromIntegral dlen) $ BSL.concat bs
      decompress comp cdata

type Archive = Map.Map String BSL.ByteString

fetchArchive :: Freenet -> RedirectTarget -> ArchiveManifestType -> IO (Either T.Text Archive)
fetchArchive fn tgt tp = do
  arch <- fetchRedirect fn tgt
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
  -> Maybe Archive                     -- ^ archive to resolve simple redirects etc against
  -> [T.Text]                          -- ^ path elements to be resolved
  -> Maybe Metadata                    -- ^ the metadata where we try to locate the entries in
  -> IO (Either T.Text BSL.ByteString) -- ^ either we fail, or we locate the final redirect step

-- redirect to default entry, which has a name of ""
resolvePath fn a [] md = print "default entry" >> resolvePath fn a [""] md

-- resolve path inside manifest
resolvePath fn a (p:ps) here@(Just (Manifest es)) = print ("manifest") >> case lookup p es of
  Nothing -> return $ Left $ "could not find path in manifest: " `T.append` p
  Just md -> case md of
    SymbolicShortlink tgt -> print tgt >> resolvePath fn a [tgt] here
    ArchiveInternalRedirect tgt _ -> case a of
      Nothing  -> return $ Left $ "AIR outside of archive"
      Just aes -> print (Map.keys aes, tgt) >> case Map.lookup (T.unpack tgt) aes of
        Nothing -> return $ Left $ "could not find " `T.append` tgt
        Just bs -> return $ Right bs
    x -> return $ Left $ T.pack $ show x -- resolvePath fn a ps $ Just md
    
-- resolve inside archive 
resolvePath fn (Just aes) [p] Nothing = case Map.lookup (T.unpack p) aes of
  Nothing -> return $ Left $ "could not find inA" `T.append` p
  Just bs -> return $ Right bs

resolvePath fn _ ps (Just (ArchiveManifest tgt at _ None)) = print ("am", ps) >> do
  arch <- fetchArchive fn tgt at
  case arch of
    Left e -> return $ Left e
    Right emap -> case Map.lookup ".metadata" emap of
      Nothing -> return $ Left $ T.pack $ "no .metadata entry found in TAR archive" ++ show emap
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
resolvePath _ _ ps md = return $ Left $ T.concat ["cannot locate ", T.pack (show ps), " in ", T.pack (show md)]

waitKeyTimeout :: DataFound f => TChan f -> Key -> IO (TMVar (Maybe f))
waitKeyTimeout chan loc = do
  bucket  <- newEmptyTMVarIO
  timeout <- registerDelay $ 10 * 1000 * 1000
  chan'    <- atomically $ dupTChan chan
  
  -- wait for data or timeout
  void $ forkIO $ atomically $ orElse
    (readTChan chan' >>= \cf -> if dataFoundLocation cf == loc then putTMVar bucket (Just cf) else retry)
    (readTVar timeout >>= \to -> if to then putTMVar bucket Nothing else retry)

  return bucket

requestData :: Freenet -> URI -> IO (Either T.Text BSL.ByteString)
requestData fn uri = let dr = toDataRequest uri in case dr of
  ChkRequest {} -> do
    let 
      l = dataRequestLocation dr
      chan = fnIncomingChk fn
      
    bucket <- waitKeyTimeout (chan) l
    handleDataRequest fn dr
    md <- atomically $ readTMVar bucket
  
    return $ case md of
      Nothing -> Left "requestData: timeout"
      Just d  -> decryptDataFound d (uriCryptoKey uri) (uriCryptoAlg uri)
      
  SskRequest {} -> do
    let 
      l = dataRequestLocation dr
      chan = fnIncomingSsk fn
      
    bucket <- waitKeyTimeout (chan) l
    handleDataRequest fn dr
    md <- atomically $ readTMVar bucket
    
    return $ case md of
      Nothing -> Left "requestData: timeout"
      Just d  -> decryptDataFound d (uriCryptoKey uri) (uriCryptoAlg uri)
      
  
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
                           Right md -> resolvePath fn Nothing (uriPath uri) (Just md)
                       else return $ Right plaintext
