
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, initFn, fetchUri
  ) where

import qualified Codec.Archive.Tar as TAR
import qualified Codec.Compression.GZip as Gzip
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void, when )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import Data.Either ( partitionEithers )
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import System.FilePath ( (</>) )

import Freenet.Chk
import qualified Freenet.Companion as FC
import Freenet.Keys
import Freenet.Metadata
import Freenet.Ssk
import qualified Freenet.Store as FS
import Freenet.Types
import Freenet.URI

data Freenet = FN
               { fnChkStore    :: FS.StoreFile ChkFound
               , fnCompanion   :: Maybe FC.Companion
               , fnIncomingChk :: TChan ChkFound
               , fnIncomingSsk :: TChan SskFound
               }

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
offerSsk fn toStore df = do
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
  print ("data request" , dr)
  
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
      case comp of
        None -> return $ Right $ cdata
        Gzip -> return $ Right $ BSL.take (fromIntegral olen) $ Gzip.decompress cdata -- FIXME: does decompress throw on illegal input? seems likely
        x    -> return $ Left $ T.pack $ "unsupported compression codec " ++ show x

-- | FIXME: watch out for infinite redirects
resolvePath
  :: Freenet
  -> [T.Text]  -- ^ path elements to be resolved
  -> Metadata  -- ^ the metadata where we try to locate the entries in
  -> IO (Either T.Text RedirectTarget) -- ^ either we fail, or we locate the final redirect step

resolvePath fn (p:ps) (Manifest es) = print (p, es) >> case lookup p es of
  Nothing -> return $ Left $ "could not find path in manifest: " `T.append` p
  Just md -> resolvePath fn ps md


resolvePath fn ps (ArchiveManifest uri _ TAR _) = do
  archive <- fetchUri fn uri
  
  case archive of
    Left e -> return $ Left e
    Right bs -> do
      let
        -- locate the ".metadata" entry in the TAR archive. heavens, why do people do this?
        mec = TAR.foldEntries
              (\e _ -> case TAR.entryPath e of
                  ".metadata" -> Just $ TAR.entryContent e
                  _           -> Nothing)
              Nothing
              (\_ -> Nothing) $ TAR.read bs
      case mec of
        Just (TAR.NormalFile tfc _) -> do
          case parseMetadata (BSL.toStrict tfc) of
            Left e -> return $ Left $ "error parsing metadata from TAR archive: " `T.append` e
            Right md -> resolvePath fn ps md
        Just _ -> return $ Left "the .metadata entry is of unknown type"
        _      -> return $ Left "no .metadata entry found in TAR archive"

resolvePath _ [] (SimpleRedirect _ tgt) = return $ Right tgt -- we're done
resolvePath _ [] (SymbolicShortlink tgt) = return $ Left tgt
resolvePath _ ps md = return $ Left $ T.concat ["cannot locate ", T.pack (show ps), " in ", T.pack (show md)]

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
      Just d  -> decryptDataFound d (uriCryptoKey uri)
  SskRequest {} -> do
    let 
      l = dataRequestLocation dr
      chan = fnIncomingSsk fn
      
    bucket <- waitKeyTimeout (chan) l
    handleDataRequest fn dr
    md <- atomically $ readTMVar bucket
  
    return $ case md of
      Nothing -> Left "requestData: timeout"
      Just d  -> decryptDataFound d (uriCryptoKey uri)
      
  
-- |
-- Tries to fetch the specified URI, parses metadata if it's
-- a control document, goes on fetching the referenced data,
-- and finally returns everything
fetchUri :: Freenet -> URI -> IO (Either T.Text BSL.ByteString)
fetchUri fn uri = do
  db <- requestData fn uri
  
  case db of
    Left e  -> return $ Left e
    Right plaintext -> if isControlDocument uri
                       then do
                         case parseMetadata (BSL.toStrict plaintext) of
                           Left e   -> return $ Left e
                           Right md -> do
                             tgt <- resolvePath fn (uriPath uri) md
                             case tgt of
                               Left e -> return $ Left e
                               Right t -> fetchRedirect fn t
                       else return $ Right plaintext
