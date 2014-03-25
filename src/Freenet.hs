
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
import Freenet.Metadata
import qualified Freenet.Store as FS
import Freenet.Types
import Freenet.URI

data DataBlock
     = ChkBlock ! ChkFound

data Freenet = FN
               { fnChkStore    :: FS.StoreFile ChkRequest ChkFound
               , fnCompanion   :: Maybe FC.Companion
               , fnIncomingChk :: TChan ChkFound
               }

-- | initializes Freenet subsystem
initFn :: CFG.Config -> IO Freenet
initFn cfg = do
  -- datastore
  dsdir       <- CFG.require cfg "datastore"
  chkStore    <- FS.mkStoreFile chkPersist (dsdir </> "store-chk") (1024 * 16)
  
  chkIncoming <- newBroadcastTChanIO
  let fn = FN chkStore Nothing chkIncoming

  -- companion
  let ccfg = CFG.subconfig "companion" cfg
  chost <- CFG.lookup ccfg "host" :: IO (Maybe String)
  case chost of
    Nothing -> return fn
    Just _  -> do
      comp <- FC.initCompanion ccfg (offerChk fn True) (error "offerSsk is missing")
      return $ fn { fnCompanion = Just comp }

offerChk :: Freenet -> Bool -> ChkFound -> STM ()
offerChk fn toStore df = do
  -- write to our store
  when toStore $ FS.putData (fnChkStore fn) df
  
  -- broadcast arrival
  writeTChan (fnIncomingChk fn) df

handleChkRequest :: Freenet -> ChkRequest -> IO ()
handleChkRequest fn dr = do
  fromStore <- FS.getData (fnChkStore fn) dr
  
  case fromStore of
    Just df -> atomically $ offerChk fn False df
    Nothing -> case fnCompanion fn of
      Nothing -> return ()
      Just c  -> FC.getChk c dr

fetchUris :: Freenet -> [URI] -> IO [(URI, Either T.Text BSL.ByteString)]
fetchUris fn uris = do
  result <- sequence $ map (fetchUri fn) uris
  return $ zip uris result
{-  let
    result = zip uris $ repeat (Left "uris: timeout")

  return result
  -}

fetchRedirect :: Freenet -> RedirectTarget -> IO (Either T.Text BSL.ByteString)
fetchRedirect fn (RedirectKey _ uri) = fetchUri fn uri
fetchRedirect fn (SplitFile comp dlen olen segs _) = do -- TODO: we're not returning the MIME
  let
    -- TODO: use FEC check blocks as well
    segUris = catMaybes $ map (\(SplitFileSegment uri isd) -> if isd then Just uri else Nothing) segs
{-
  -- register data handlers
  dhs <- mapM (\uri -> timeoutHandler fn $ uriLocation uri) segUris
  
  -- schedule requests
  mapM_ (requestData fn) segUris
  
  -- wait for data to arrive (or timeout)
  ds <- mapM (\(DataHandler bucket) -> atomically $ takeTMVar bucket) dhs
  -}
  ds <- fetchUris fn segUris
  
  -- see if everything could be fetched and try to decrypt
  let
--    keys = map chkKey segUris
--    dec (Left e)   _   = Left e
--    dec (Right df) key = decryptChk key df
--    ps = map (\(mdf, key) -> dec mdf key) $ zip (map snd ds) keys
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

requestData :: Freenet -> URI -> IO (Either T.Text BSL.ByteString)
requestData fn (CHK l ck e _) = do
  bucket  <- newEmptyTMVarIO
  timeout <- registerDelay $ 10 * 1000 * 1000
  chan    <- atomically $ dupTChan $ fnIncomingChk fn
  
  -- wait for data or timeout
  void $ forkIO $ atomically $ orElse
    (readTChan chan >>= \cf -> if dataFoundLocation cf == l then putTMVar bucket (Just cf) else retry)
    (readTVar timeout >>= \to -> if to then putTMVar bucket Nothing else retry)
    
  handleChkRequest fn $ ChkRequest l $ chkExtraCrypto e

  md <- atomically $ readTMVar bucket

  return $ case md of
    Nothing -> Left "requestData: timeout"
    Just d  -> decryptChk ck d

-- |
-- Tries to fetch the specified URI, parses metadata if it's
-- a control document, goes on fetching the referenced data,
-- and finally returns everything
fetchUri :: Freenet -> URI -> IO (Either T.Text BSL.ByteString)
fetchUri fn uri = do
  let
--    dr = toDataRequest uri
    key = uriCryptoKey uri
    path = uriPath uri
  
--  (DataHandler bucket) <- timeoutHandler fn $ uriLocation uri
  db <- requestData fn uri
 -- df <- atomically $ takeTMVar bucket
  
  case db of
    Left e  -> return $ Left e
    Right plaintext -> if isControlDocument uri
                       then do
                         case parseMetadata (BSL.toStrict plaintext) of
                           Left e   -> return $ Left e
                           Right md -> do
                             tgt <- resolvePath fn path md
                             case tgt of
                               Left e -> return $ Left e
                               Right t -> fetchRedirect fn t
                       else return $ Right plaintext
