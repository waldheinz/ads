
{-# LANGUAGE OverloadedStrings #-}

module Freenet (
  Freenet, initFn, fetchUri
  ) where

import qualified Codec.Archive.Tar as TAR
import qualified Codec.Compression.GZip as Gzip
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( void )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import Data.Either ( partitionEithers )
import qualified Data.Map.Strict as Map
import Data.Maybe ( catMaybes )
import qualified Data.Text as T

import qualified Freenet.Companion as FC
import qualified Freenet.Data as FD
import Freenet.Metadata
import qualified Freenet.Store as FS
import Freenet.Types
import qualified Freenet.URI as FU

newtype DataHandler = DataHandler (TMVar (Either T.Text FD.DataFound)) deriving ( Eq )

data Freenet = FN
               { fnStore     :: FS.FileStore
               , fnCompanion :: Maybe FC.Companion
               , fnRequests  :: TVar (Map.Map Key [DataHandler])
               }

-- | initializes Freenet subsystem
initFn :: CFG.Config -> IO Freenet
initFn cfg = do
  -- datastore
  dsdir <- CFG.require cfg "datastore"
  chkStore <- FS.mkFileStore 1024 dsdir
  reqs <- newTVarIO Map.empty
  
  let fn = FN chkStore Nothing reqs

  -- companion
  let ccfg = CFG.subconfig "companion" cfg
  chost <- CFG.lookup ccfg "host" :: IO (Maybe String)
  case chost of
    Nothing -> return fn
    Just _  -> do
      comp <- FC.initCompanion ccfg (offer fn)
      return $ fn { fnCompanion = Just comp }

offer :: Freenet -> FD.DataFound -> STM ()
offer fn df = do
  -- write to our store
  FS.putData (fnStore fn) df

  -- inform other registered handlers
  m <- readTVar (fnRequests fn)
  inform $ Map.lookup key m
  modifyTVar (fnRequests fn) (Map.delete key)
  where
    key = FD.dataFoundLocation df
    inform Nothing = return ()
    inform (Just dhs) = mapM_ (\(DataHandler bucket) -> putTMVar bucket (Right df)) dhs

addHandler :: Freenet -> Key -> DataHandler -> STM ()
addHandler fn key dh = modifyTVar (fnRequests fn) $ Map.insertWith (++) key [dh]

removeHandler :: Freenet -> Key -> DataHandler -> STM ()
removeHandler fn key dh = modifyTVar (fnRequests fn) $ Map.update u key where
  u dhs = if null dhs' then Nothing else Just dhs' where
    dhs' = filter (== dh) dhs

-- | creates a dataHandler which will set it's bucket to Nothing after a timeout
timeoutHandler :: Freenet -> Key -> IO DataHandler
timeoutHandler fn key = do
  timeout <- registerDelay (1000 * 1000 * 10)
  
  dh@(DataHandler bucket) <- atomically $ do
    dfc <- newEmptyTMVar
    let dh = DataHandler dfc
    addHandler fn key dh
    return dh
  
  void $ forkIO $ atomically $ do
    empty <- isEmptyTMVar bucket

    if empty
      then readTVar timeout >>= \to -> if to
                                       then putTMVar bucket (Left "timeout") >> removeHandler fn key dh
                                       else retry
      else removeHandler fn key dh
    
  return dh
  
handleRequest :: Freenet -> DataRequest -> IO ()
handleRequest fn dr = do
  fromStore <- FS.getData (fnStore fn) dr
  
  case fromStore of
    Just df -> atomically $ offer fn df
    Nothing -> case fnCompanion fn of
      Nothing -> return ()
      Just c  -> FC.getData c dr

fetchRedirect :: Freenet -> RedirectTarget -> IO (Either T.Text BSL.ByteString)
fetchRedirect fn (SplitFile comp dlen olen segs _) = do -- TODO: we're not returning the MIME
  let
    -- TODO: use FEC check blocks as well
    segUris = catMaybes $ map (\(SplitFileSegment uri isd) -> if isd then Just uri else Nothing) segs

  -- register data handlers
  dhs <- mapM (\uri -> timeoutHandler fn $ FU.uriLocation uri) segUris
  
  -- schedule requests
  mapM_ (\uri -> print uri >> (handleRequest fn $ FU.toDataRequest uri)) segUris
  
  -- wait for data to arrive (or timeout)
  ds <- mapM (\(DataHandler bucket) -> atomically $ takeTMVar bucket) dhs
  
  -- see if everything could be fetched and try to decrypt
  let
    keys = map FU.chkKey segUris
    dec (Left e)   _   = Left e
    dec (Right df) key = FD.decryptDataFound key df
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
              (\e x -> case TAR.entryPath e of
                  ".metadata" -> Just $ TAR.entryContent e
                  x           -> Nothing)
              Nothing
              (\e -> Nothing) $ TAR.read bs
      case mec of
        Just (TAR.NormalFile bs sz) -> do
          case parseMetadata (BSL.toStrict bs) of
            Left e -> return $ Left $ "error parsing metadata from TAR archive: " `T.append` e
            Right md -> resolvePath fn ps md
        Just _ -> return $ Left "the .metadata entry is of unknown type"
        _      -> return $ Left "no .metadata entry found in TAR archive"
        
--  return $ Left "am"

resolvePath _ [] (SimpleRedirect _ tgt) = return $ Right tgt -- we're done

resolvePath _ ps md = return $ Left $ T.concat ["cannot locate ", T.pack (show ps), " in ", T.pack (show md)]

-- |
-- Tries to fetch the specified URI, possibly parsed metadata if it's
-- a control document, goes on fetching the referenced data if it was
-- and finally returns everything
fetchUri :: Freenet -> FU.URI -> IO (Either T.Text BSL.ByteString)
fetchUri fn uri = do
  let
    dr = FU.toDataRequest uri
    key = FU.chkKey uri
    path = FU.uriPath uri
  
  (DataHandler bucket) <- timeoutHandler fn $ FU.uriLocation uri
  handleRequest fn dr
  df <- atomically $ takeTMVar bucket
  
  case df of
    Left e  -> return $ Left e
    Right d -> case FD.decryptDataFound key d of
      Left e          -> return $ Left e
      Right plaintext -> if FU.isControlDocument uri
                         then do
                           case parseMetadata plaintext of
                             Left e   -> return $ Left e
                             Right md -> do
                               tgt <- resolvePath fn path md
                               case tgt of
                                 Left e -> return $ Left e
                                 Right t -> fetchRedirect fn t
                         else return $ Right (BSL.fromStrict plaintext)
