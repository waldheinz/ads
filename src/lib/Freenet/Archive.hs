
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Archive (
  Archive, fetchArchive,
  ArchiveCache, mkArchiveCache
  ) where

import qualified Codec.Archive.Tar as TAR
import qualified Codec.Archive.Zip as ZIP
import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import qualified Control.Exception as CE
import Control.Monad ( when, void )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Cache.LRU as LRU
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import System.Log.Logger

import Freenet.Metadata
import Freenet.SplitFile
import Freenet.URI
import Utils

-- |
-- An Archive is just a map from file names to file contents.
type Archive = Map.HashMap String BSL.ByteString

logD :: String -> IO ()
logD m = debugM "freenet.archive" m

newtype ArchiveKey = ArchiveKey (RedirectTarget, ArchiveType) deriving ( Eq, Ord )

newtype ArchiveProgress = ArchiveProgress { unProgress :: TMVar (Either T.Text Archive) }

data ArchiveCache = ArchiveCache
                    { acLru :: TVar (LRU.LRU ArchiveKey ArchiveProgress)
                    }

mkArchiveCache :: Integer -> IO ArchiveCache
mkArchiveCache size = do
  lru <- newTVarIO $ LRU.newLRU $ Just size
  return $ ArchiveCache lru

fetchArchive :: UriFetch a => a -> ArchiveCache -> RedirectTarget -> ArchiveType -> IO (Either T.Text Archive)
fetchArchive fetch ac tgt tp = do
  let ak = ArchiveKey (tgt, tp)
      
  (prog, needStart) <- atomically $ do
    lru <- readTVar (acLru ac)
    let (lru', mprog) = LRU.lookup ak lru
    
    case mprog of
      Just p  -> writeTVar (acLru ac) lru' >> return (p, False)
      Nothing -> do
        p <- ArchiveProgress <$> newEmptyTMVar
        writeTVar (acLru ac) $ LRU.insert ak p lru
        return (p, True)
  
  when needStart $ void $ forkIO $ do
    arch <- fetchArchive' fetch tgt tp
    atomically $ do
      putTMVar (unProgress prog) arch
      -- drop failures from cache so they can be fetched again if the user dares
      case arch of
        Left _ -> modifyTVar' (acLru ac) $ \lru -> let (lru', _) = LRU.delete ak lru in lru'
        _       -> return ()
    

  atomically $ readTMVar (unProgress prog)

fetchArchive' :: UriFetch a => a -> RedirectTarget -> ArchiveType -> IO (Either T.Text Archive)
fetchArchive' fetch tgt tp = do
  logD $ "fetching archive " ++ show tgt
  
  arch <- fetchRedirect' fetch tgt

  let
    parseZip zbs = CE.catch (return $ Right go) handler where
      handler :: CE.ErrorCall -> IO (Either T.Text Archive)
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

fetchRedirect' :: UriFetch a => a -> RedirectTarget -> IO (Either T.Text BSL.ByteString)
fetchRedirect' fetch (RedirectKey _ uri) = do
  logD $ "fetch redirect' to " ++ show uri
  
  mbs <- getUriData fetch uri
  case mbs of
    Left e   -> return $ Left $ "fetchRedirect': error with requestNodeData: " `T.append` e
    Right (bs, len) -> case parseMetadata (BSL.take (fromIntegral len) $ bsFromStrict bs) of
      Left _  -> return $ Right $ BSL.take (fromIntegral len) $ bsFromStrict bs
      Right md -> case md of
        ArchiveManifest (RedirectSplitFile sf) _ _ _ -> fetchSplitFile fetch sf
        _ -> return $ Left $ "fetchRedirect': what shall I do with metadata: " `T.append` (T.pack $ show md)

fetchRedirect' fetch (RedirectSplitFile sf) = fetchSplitFile fetch sf
