
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
import qualified Data.Map.Strict as Map
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import System.FilePath ( (</>) )

import Freenet.Bucket
import Freenet.Chk
import qualified Freenet.Companion as FC
import Freenet.Metadata
import qualified Freenet.Store as FS
import Freenet.Types
import Freenet.URI

-- |
-- 
type KeyToBucket d = Map.Map Key (MemoryBucket (Maybe d))

data Freenet = FN
               { fnChkStore   :: FS.StoreFile
               , fnCompanion  :: Maybe FC.Companion
               , fnChkBuckets :: TVar (KeyToBucket ChkFound)
               }

-- | initializes Freenet subsystem
initFn :: CFG.Config -> IO Freenet
initFn cfg = do
  -- datastore
  dsdir <- CFG.require cfg "datastore"
  chkStore <- FS.mkStoreFile (1024 * 16) $ dsdir </> "store-chk"
  reqs <- newMemoryBucket
  
  let fn = FN chkStore Nothing reqs

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

  -- put the data into the bucket (if there is one)
  m <- readTVar (fnChkBuckets fn)
  inform $ Map.lookup key m
  modifyTVar (fnChkBuckets fn) (Map.delete key)
  where
    key = dataFoundLocation df
    inform Nothing = return ()
    inform (Just bucket) = putBucket bucket $ Just df

{-
addHandler :: Freenet -> Key -> DataHandler -> STM ()
addHandler fn key dh = modifyTVar (fnRequests fn) $ Map.insertWith (++) key [dh]

removeHandler :: Freenet -> Key -> DataHandler -> STM ()
removeHandler fn key dh = modifyTVar (fnRequests fn) $ Map.update u key where
  u dhs = if null dhs' then Nothing else Just dhs' where
    dhs' = filter (== dh) dhs
-}

handleChkRequest :: Freenet -> ChkRequest -> IO ()
handleChkRequest fn dr = do
  fromStore <- FS.getData (fnChkStore fn) dr
  
  case fromStore of
    Just df -> atomically $ offerChk fn False df
    Nothing -> case fnCompanion fn of
      Nothing -> return ()
      Just c  -> FC.getChk c dr

fetchRedirect :: Freenet -> RedirectTarget -> IO (Either T.Text BSL.ByteString)
fetchRedirect fn (RedirectKey _ uri) = fetchUri fn uri
fetchRedirect fn (SplitFile comp dlen olen segs _) = do -- TODO: we're not returning the MIME
  let
    -- TODO: use FEC check blocks as well
    segUris = catMaybes $ map (\(SplitFileSegment uri isd) -> if isd then Just uri else Nothing) segs

  -- register data handlers
  dhs <- mapM (\uri -> timeoutHandler fn $ uriLocation uri) segUris
  
  -- schedule requests
  mapM_ (requestData fn) segUris
  
  -- wait for data to arrive (or timeout)
  ds <- mapM (\(DataHandler bucket) -> atomically $ takeTMVar bucket) dhs
  
  -- see if everything could be fetched and try to decrypt
  let
    keys = map chkKey segUris
    dec (Left e)   _   = Left e
    dec (Right df) key = decryptChk key df
    ps = map (\(mdf, key) -> dec mdf key) $ zip ds keys
    (es, bs) = partitionEithers ps

  if (not . null) es
    then return $ Left $ T.intercalate ", " es
    else do
      let cdata = BSL.take (fromIntegral dlen) $ BSL.fromChunks bs
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

requestData :: Freenet -> URI -> IO (Maybe ChkFound)
requestData fn (CHK l _ e _) = do
  atomically $ do
    m <- readTVar (fnChkBuckets fn)
    case Map.lookup l m of
      Nothing -> newMemoryBucket
                 (\mb -> writeTVar (fnChkBuckets fn) $ Map.delete l) -- cleanup
                 (\mb -> 
    modifyTVar (fnChkBuckets fn) $ Map.insertWith (++) key [dh]
    
  handleChkRequest fn $ ChkRequest l e

--waitData :: Freenet -> Key 

-- |
-- Tries to fetch the specified URI, possibly parsed metadata if it's
-- a control document, goes on fetching the referenced data if it was
-- and finally returns everything
fetchUri :: Freenet -> URI -> IO (Either T.Text BSL.ByteString)
fetchUri fn uri = do
  let
--    dr = toDataRequest uri
    key = uriCryptoKey uri
    path = uriPath uri
  
  (DataHandler bucket) <- timeoutHandler fn $ uriLocation uri
  requestData fn uri
  df <- atomically $ takeTMVar bucket
  
  case df of
    Left e  -> return $ Left e
    Right d -> case decryptBlock key d of
      Left e          -> return $ Left e
      Right plaintext -> if isControlDocument uri
                         then do
                           case parseMetadata plaintext of
                             Left e   -> return $ Left e
                             Right md -> do
                               tgt <- resolvePath fn path md
                               case tgt of
                                 Left e -> return $ Left e
                                 Right t -> fetchRedirect fn t
                         else return $ Right (BSL.fromStrict plaintext)
