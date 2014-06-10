
module Requests (
  -- * Requesting Data

  UriFetch(..)
  
  -- * Inserting Data
  
  ) where

import           Control.Concurrent ( forkIO )
import           Control.Concurrent.STM
import           Control.Monad ( void, when )
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T

import qualified Freenet.Chk as FN
import qualified Freenet.Ssk as FN
import qualified Freenet.Types as FN
import qualified Freenet.URI as FN
import           Message
import           Node

class UriFetch a where
  getUriData :: a -> FN.URI -> IO (Either T.Text (BS.ByteString, Int))



-------------------------------------------------------------------------------------------------
-- Organizing data requests
-------------------------------------------------------------------------------------------------

data Delayed d = Delayed ! (TMVar (Maybe d))

waitDelayed :: Delayed d -> STM (Maybe d)
waitDelayed (Delayed d) = readTMVar d

data RequestManager r d = RequestManager
                        { rmRequests :: ! (TVar (HMap.HashMap FN.Key (Delayed d)))
                        , rmTimeout  :: ! Int
                        }

mkRequestManager :: STM (RequestManager r d)
mkRequestManager = do
  reqs <- newTVar HMap.empty
  return $! RequestManager reqs (30 * 1000 * 1000)

offer :: (FN.DataBlock d) => d -> RequestManager r d -> STM ()
offer db rmgr = do

  let
    key = FN.dataBlockLocation db
  
  rm <- readTVar (rmRequests rmgr)

  case HMap.lookup key rm of
    Nothing          -> return ()
    Just (Delayed d) -> do
      putTMVar d (Just db)
      writeTVar (rmRequests rmgr) $ HMap.delete key rm

request :: (FN.DataRequest r) => RequestManager r d -> r -> (r -> IO ()) -> IO (Delayed d)
request rmgr dr act = do
  let
    key = FN.dataRequestLocation dr
    checkTimeout (Delayed d) to = orElse
      (isEmptyTMVar d >>= \e -> when e retry)
      (readTVar to    >>= \t -> if t
                                then putTMVar d Nothing >> modifyTVar' (rmRequests rmgr) (HMap.delete key)
                                else retry)
                   
  (result, needStart) <- atomically $ do
    rm <- readTVar (rmRequests rmgr)
    case HMap.lookup key rm of
      Just old -> return (old, False)   -- request is already running
      Nothing  -> do                    -- make new Delayed
        b <- newEmptyTMVar
        let d = Delayed b
        writeTVar (rmRequests rmgr) $ HMap.insert key d rm
        return (d, True)
  
  when needStart $ do
    to <- registerDelay $ rmTimeout rmgr
    void $ forkIO $ atomically $ checkTimeout result to
    act dr
    
  return result
  
------------------------------------------------------------------------------------------
-- fetching data
------------------------------------------------------------------------------------------
{-
nodeFetchChk :: PeerAddress a => Node a -> FN.ChkRequest -> ((Either T.Text FN.ChkBlock) -> IO b) -> IO b
nodeFetchChk node req k = do
  fromStore <- FN.getChk (nodeFreenet node) req

  case fromStore of
    Right blk -> k $ Right blk
    Left  _   -> do
      d <- request (nodeChkRequests node) req $ \r -> do
        mkRoutedMessage node (FN.dataRequestLocation req) (FreenetChkRequest r)

      result <- atomically $ waitDelayed d
      
      case result of
        Nothing  -> k $ Left "timeout waiting for CHK data"
        Just blk -> k $ Right blk

nodeFetchSsk :: PeerAddress a => Node a -> FN.SskRequest -> ((Either T.Text FN.SskBlock) -> IO b) -> IO b
nodeFetchSsk node req k = do
  fromStore <- FN.getSsk (nodeFreenet node) req

  case fromStore of
    Right blk -> k $ Right blk
    Left  _   -> do
      d <- request (nodeSskRequests node) req $ \r -> do
        mkRoutedMessage node (FN.dataRequestLocation req) (FreenetSskRequest r)

      result <- atomically $ waitDelayed d
      
      case result of
        Nothing  -> k $ Left "timeout waiting for SSK data"
        Just blk -> k $ Right blk
                    
requestNodeData :: PeerAddress a => Node a -> FN.URI -> IO (Either T.Text (BS.ByteString, Int))

requestNodeData n (FN.CHK loc key extra _) =
  case FN.chkExtraCompression extra of
    Left  e -> return $ Left $ "can't decompress CHK: " `T.append` e
    Right c -> nodeFetchChk n (FN.ChkRequest loc $ FN.chkExtraCrypto extra) $ \result ->
      case result of
        Left e    -> return $ Left e
        Right blk -> decrypt blk where
          decrypt b = case FN.decryptDataBlock b key of
            Left e        -> return $ Left $ "decrypting CHK data block failed: " `T.append` e
            Right (p, pl) -> FN.decompressChk c p pl
        
requestNodeData n (FN.SSK pkh key extra dn _) = do
  let
    req = FN.SskRequest pkh (FN.sskEncryptDocname key dn) (FN.sskExtraCrypto extra)
    decrypt blk = FN.decryptDataBlock blk key
        
  fromStore <- FN.getSsk (nodeFreenet n) req

  case fromStore of
    Right blk -> return $ decrypt blk -- (BSL.take (fromIntegral bl) $ BSL.fromStrict blk)
    Left _    -> do
      d <- request (nodeSskRequests n) req $ \r -> do
        mkRoutedMessage n (FN.dataRequestLocation req) (FreenetSskRequest r)
          
      result <- atomically $ waitDelayed d
          
      case result of
        Nothing  -> return $ Left "timeout waiting for SSK data"
        Just blk -> return $ decrypt blk

requestNodeData n (FN.USK pkh key extra dn dr _) = do
  let
    dn' = dn `T.append` "-" `T.append` T.pack (show dr)
    req = FN.SskRequest pkh (FN.sskEncryptDocname key dn') (FN.sskExtraCrypto extra)
    decrypt blk = FN.decryptDataBlock blk key
        
  fromStore <- FN.getSsk (nodeFreenet n) req

  case fromStore of
    Right blk -> return $ decrypt blk
    Left _    -> do
      d <- request (nodeSskRequests n) req $ \r -> do
        mkRoutedMessage n
          (FN.dataRequestLocation req)
          (FreenetSskRequest r)
          
      result <- atomically $ waitDelayed d
          
      case result of
        Nothing  -> return $ Left "timeout waiting for USK data"
        Just blk -> return $ decrypt blk

-}
