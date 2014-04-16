
{-# LANGUAGE OverloadedStrings #-}

module Node (
  -- * our node
  Node, mkNode, readPeers,
  requestNodeData, nodeArchives, nodePeers,
  nodeFreenet, nodeRouteStatus,
  
  -- * peers
  ConnectFunction,
  
  -- * other nodes we're connected to
  PeerNode, runPeerNode  
  ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO, killThread )
import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBMQueue as STM
import Control.Monad ( forever, unless, void, when )
import Control.Monad.IO.Class ( liftIO )

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.TQueue as C
import Data.List ( nub )
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import Data.Maybe ( isJust )
import qualified Data.Text as T
import System.FilePath ( (</>) )
import System.Log.Logger

import qualified Freenet as FN
import qualified Freenet.Archive as FN
import qualified Freenet.Chk as FN
import qualified Freenet.Ssk as FN
import qualified Freenet.Types as FN
import qualified Freenet.URI as FN
import Message as MSG
import qualified NextBestOnce as NBO
import Peers
import Time
import Types

logD :: String -> IO ()
logD = debugM "node"

logI :: String -> IO ()
logI = infoM "node"

logW :: String -> IO ()
logW = warningM "node"

-------------------------------------------------------------------------
-- Node
-------------------------------------------------------------------------

data Node a = Node
            { nodePeers       :: TVar [Peer a]                                  -- ^ the peers we know about
            , nodeConnecting  :: TVar [Peer a]                                  -- ^ peers we're currently connecting to
            , nodePeerNodes   :: TVar [PeerNode a]                              -- ^ the nodes we're connected to
            , nodeIdentity    :: NodeInfo a                                     -- ^ our identity
            , nodeMidGen      :: MessageIdGen                                   -- ^ message id generator
            , nodeActMsgs     :: TVar (Map.Map MessageId (ActiveMessage a))     -- ^ messages we're currently routing
            , nodeNbo         :: NBO.Node NodeId (RoutedMessage a) (PeerNode a) -- ^ our NBO identity for routing
            , nodeFreenet     :: FN.Freenet a                                   -- ^ our freenet compatibility layer
            , nodeArchives    :: FN.ArchiveCache                                -- ^ Freenet LRU archive cache
            , nodeChkRequests :: RequestManager FN.ChkRequest FN.ChkBlock
            , nodeSskRequests :: RequestManager FN.SskRequest FN.SskBlock
            }

mkNode :: PeerAddress a => NodeInfo a -> FN.Freenet a -> ConnectFunction a -> IO (Node a)
mkNode self fn connect = do
  peers  <- newTVarIO []
  connecting <- newTVarIO [] -- peers we're currently connecting to
  pns    <- newTVarIO []
  midgen <- mkMessageIdGen
  msgMap <- newTVarIO Map.empty
  ac     <- FN.mkArchiveCache 10
  chkRm  <- atomically mkRequestManager
  sskRm  <- atomically mkRequestManager

  let
    nbo = NBO.Node
        { NBO.location          = nodeId self
        , NBO.neighbours        = readTVar pns
        , NBO.neighbourLocation = peerId . pnPeer
        , NBO.popPred           = messagePopPred node . rmId
        , NBO.pushPred          = messagePushPred node . rmId
        , NBO.routingInfo       = rmInfo
        , NBO.updateRoutingInfo = \rm ri -> rm { rmInfo = ri }
        }
        
    node = Node peers connecting pns self midgen msgMap nbo fn ac chkRm sskRm

  void $ forkIO $ maintainConnections node connect
  
  return node

handlePeerMessages :: PeerAddress a => Node a -> PeerNode a -> Message a -> IO ()

handlePeerMessages node pn (Direct GetPeerList) = atomically $ do
  nis <- readTVar (nodePeers node) >>= mapM mkNodeInfo
  enqMessage pn $ Direct $ PeerList nis

handlePeerMessages node _  (Direct (PeerList ps)) = do
  logD $ "got some peers: " ++ show ps
  atomically $ mapM_ (mergeNodeInfo node) ps
  
handlePeerMessages node pn msg = do
  let
    fn = nodeFreenet node
    
    route = void $ forkIO $ case msg of
      Routed False rm@(RoutedMessage (FreenetChkRequest req) mid _) -> do
        local <- FN.getChk fn req
        case local of
          Left _    -> sendRoutedMessage node rm (Just pn) -- pass on
          Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetChkBlock blk

      Routed False rm@(RoutedMessage (FreenetSskRequest req) mid _) -> do
        local <- FN.getSsk fn req
        case local of
          Left _    -> sendRoutedMessage node rm (Just pn) -- pass on
          Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetSskBlock blk
          
      Routed True rm    -> sendRoutedMessage node rm Nothing -- backtrack
      Response mid msg' -> forwardResponse node mid msg'

      _ -> return ()

    writeStores = case msg of
      Response _ (FreenetChkBlock blk) -> do
        atomically $ offer blk (nodeChkRequests node)
        FN.offerChk (nodeFreenet node) blk
        
      Response _ (FreenetSskBlock blk) -> do
        atomically $ offer blk (nodeSskRequests node)
        FN.offerSsk (nodeFreenet node) blk
      _   -> return ()
    
  writeStores >> route

-----------------------------------------------------------------------------------------------
-- Routing
-----------------------------------------------------------------------------------------------

data ActiveMessage a = ActiveMessage
                       { amStarted :: Timestamp     -- ^ when this message was sent off
                       , amPreds   :: [PeerNode a]  -- ^ predecessors (where the message came from)
                       }

-- |
-- Turn a Freenet @Key@ into a @NodeId@ by repacking the 32 bytes.
keyToTarget :: FN.Key -> NodeId
keyToTarget key = mkNodeId' $ FN.unKey key

mkRoutedMessage :: Node a -> NodeId -> MessagePayload a -> IO (RoutedMessage a)
mkRoutedMessage node target msg = atomically $ do
  mid <- nextMessageId $ nodeMidGen node
  return $ RoutedMessage msg mid $ NBO.mkRoutingInfo target

sendRoutedMessage :: Show a => Node a -> RoutedMessage a -> Maybe (PeerNode a) -> IO ()
sendRoutedMessage node msg prev = do
  result <- NBO.route (nodeNbo node) prev msg
    
  case result of
    NBO.Forward dest msg'   -> atomically $ enqMessage dest (Routed False msg')
    NBO.Backtrack dest msg' -> atomically $ enqMessage dest (Routed True  msg')
    NBO.Fail                -> logI $ "message failed fatally: " ++ show msg

messagePushPred :: Node a -> MessageId -> PeerNode a -> IO ()
messagePushPred node mid pn = do
  now <- getTime
  atomically $ modifyTVar' (nodeActMsgs node) $ Map.insertWith prepend mid $ ActiveMessage now [pn]
  where
    prepend (ActiveMessage _ (x:_)) (ActiveMessage sent xs) = ActiveMessage sent (x:xs)
    prepend _ _ = error "messagePushPred: error in prepend"

messagePopPred :: Node a -> MessageId -> STM (Maybe (PeerNode a))
messagePopPred node mid = do
  let
    update _ (ActiveMessage _     [])    = Nothing -- should not happen because 
    update _ (ActiveMessage _    (_:[])) = Nothing -- of this
    update _ (ActiveMessage sent (_:xs)) = Just $ ActiveMessage sent xs

  (tgt, m') <- Map.updateLookupWithKey update mid <$> readTVar (nodeActMsgs node)
  writeTVar (nodeActMsgs node) m'
  
  case tgt of
    Just (ActiveMessage _ (x:_)) -> return $ Just x
    _                            -> return Nothing

forwardResponse :: Node a -> MessageId -> MessagePayload a -> IO ()
forwardResponse node mid msg = do
  mtgt <- atomically $ messagePopPred node mid
  
  case mtgt of
    Nothing -> logW $ "could not send response, message id unknown: " ++ show mid
    Just pn -> atomically $ enqMessage pn $ Response mid msg

nodeRouteStatus :: Node a -> IO Value
nodeRouteStatus node = do
  now <- getTime

  let
    toState xs mid am = x : xs where
      x = object
          [ "messageId" .= mid
          , "age"       .= timeDiff (amStarted am) now
          ]
  
  msgs <- Map.foldlWithKey' toState [] <$> atomically (readTVar $ nodeActMsgs node)
  
  return $ object [ "messages" .= msgs ]
    
----------------------------------------------------------------
-- the peers list
----------------------------------------------------------------

type ConnectFunction a = Peer a -> (Either String (MessageIO a) -> IO ()) -> IO ()

readPeers
  :: (PeerAddress a)
  => Node a          -- ^ our node
  -> FilePath        -- ^ app data directory containing the peers file
  -> IO ()
readPeers node dataDir = do
  let
    kpFile = dataDir </> "peers"
    
  logI $ "reading known peers from " ++ kpFile
  kpbs <- BSL.readFile kpFile
  
  case eitherDecode kpbs of
    Left  e     -> logW ("error parsing peers file: " ++ e)
    Right peers -> do
      logI ("got " ++ show (length peers) ++ " peers")
      atomically $ mapM_ (mergeNodeInfo node) peers

-- |
-- Merges the information about some peer with our set of known peers,
-- adding new ones or just updating addresses of the ones we already
-- know about.
mergeNodeInfo :: PeerAddress a => Node a -> NodeInfo a -> STM ()
mergeNodeInfo node ni = unless (nodeId ni == (nodeId . nodeIdentity) node) $ do
  p   <- mkPeer ni

  let
    merge (Peer _ av1) (Peer _ av2) = do
      as2 <- readTVar av2
      modifyTVar' av1 $ \as1 -> nub (as1 ++ as2)
  
    go [] = return [p]
    go (x:xs)
      | x == p = merge x p >> return (x:xs)
      | otherwise = go xs >>= \xs' -> return (x:xs')
  
  ps  <- readTVar $ nodePeers node
  ps' <- go ps
  writeTVar (nodePeers node) ps'
  
-- |
-- Adds a peer to the set of connected peers. It is now a partner
-- for message exchange, instead of just someone we know of.
addPeerNode :: PeerAddress a => Node a -> PeerNode a -> STM (Maybe T.Text)
addPeerNode node pn = do
  mkNodeInfo (pnPeer pn) >>= mergeNodeInfo node
  connected <- readTVar $ nodePeerNodes node
  
  if pn `elem` connected -- this can happen on simultaneous connect from both sides
    then return $ Just "already connected"
    else writeTVar (nodePeerNodes node) (pn : connected) >> return Nothing

-- |
-- Removed a peer from the set of connected peers, when we decided
-- we don't want to talk to this peer any more, or the connection
-- was lost.
removePeerNode :: Node a -> PeerNode a -> STM ()
removePeerNode node pn = do
  modifyTVar' (nodePeerNodes node) $ filter (/= pn)         -- ^ it's no longer connected
  modifyTVar' (nodeActMsgs node)   $ Map.mapMaybe dropPreds -- ^ and we can drop messages routed for this node
  where
    dropPreds am
      | null xs'  = Nothing
      | otherwise = Just am { amPreds = xs' }
      where
        xs' = filter (/= pn) (amPreds am)
        
maintainConnections :: PeerAddress a => Node a -> ConnectFunction a -> IO ()
maintainConnections node connect = forever $ do
  -- we simply try to maintain a connection to all known peers for now
  delay <- registerDelay $ 2 * 1000 * 1000 -- limit outgoing connection rate to 2 per second

  shouldConnect <- atomically $ do
    readTVar delay >>= check
    
    known <- readTVar $ nodePeers node
    cting <- readTVar $ nodeConnecting node
    connected <- readTVar $ nodePeerNodes node
    
    let
      cpeers = map pnPeer connected
      result = [x | x <- known, not $ x `elem` cting, not $ x `elem` cpeers]

    if null result
      then retry
      else do
        let result' = head result
        modifyTVar' (nodeConnecting node) ((:) result')
    
        return result'

  logD $ "connecting to " ++ show (peerId shouldConnect)
  atomically $ modifyTVar' (nodePeers node) (\pls -> case pls of
                                                [] -> []
                                                (p:ps) -> ps ++ [p])

  void $ forkIO $ connect shouldConnect $ \cresult -> do
    atomically $ modifyTVar' (nodeConnecting node) (filter (/= shouldConnect))
    case cresult of
      Left _      -> return ()
      Right msgio -> runPeerNode node msgio (Just $ peerId shouldConnect)

-------------------------------------------------------------------------
-- Peer Nodes
-------------------------------------------------------------------------

-- |
-- A @PeerNode@ is a @Peer@ we're currently connected to.
data PeerNode a
  = PeerNode
    { pnPeer        :: Peer a               -- ^ the peer
    , pnQueue       :: TBMQueue (Message a) -- ^ outgoing message queue
    , pnLastMessage :: TVar Timestamp       -- ^ when the last message was received
    }
    
instance (Show a) => Show (PeerNode a) where
  show n = "Node {peer = " ++ show (peerId $ pnPeer n) ++ " }"

instance Eq (PeerNode a) where
  (PeerNode p1 _ _) == (PeerNode p2 _ _) = p1 == p2

instance ToJSON a => ToStateJSON (PeerNode a) where
  toStateJSON pn = do
--    lm <- readTVar $ pnLastMessage pn
    p <- toStateJSON pn
    return $ object
      [ "peer"        .= p
--     , "lastMessage" .= (fromIntegral lm)
      ]

-- | puts a message on the Node's outgoing message queue       
enqMessage :: PeerNode a -> Message a -> STM ()
enqMessage n = writeTBMQueue (pnQueue n)

{-
doPing :: PeerNode a -> IO ()
doPing pn = do
  to <- registerDelay $ 10 * 1000 * 1000
  now <- getPOSIXTime
  
  atomically $ orElse
    (readTVar (pnLastMessage pn) >>= \last -> if (now - last < 20) then retry else return ())
    (readTVar to >>= \timeout -> if timeout then enqMessage pn (Direct Ping) else retry)

-}

------------------------------------------------------------------------------
-- Handshake
------------------------------------------------------------------------------

-- ^ handle a node that is currently connected to us
runPeerNode
  :: (PeerAddress a)
  => Node a
  -> MessageIO a    -- ^ the (source, sink) pair to talk to the peer
  -> Maybe (NodeId)
  -> IO ()
runPeerNode node (src, sink) expected = do
  mq <- newTBMQueueIO 50
  
  -- enqueue the obligatory Hello message, if this is an outgoing connection
  when (isJust expected) $ atomically $ writeTBMQueue mq (Direct $ Hello $ nodeIdentity node)
  
  tid <- forkIO $ src C.$$ C.awaitForever $ \msg ->
    case msg of
      Direct (Hello ni) -> do
        case expected of
          Nothing -> return ()
          Just e -> when (e /= (nodeId ni)) $ error "node identity mismatch"
          
        pn <- liftIO $ do
          p <- atomically $ mkPeer ni
          getTime >>= newTVarIO >>= \now -> return (PeerNode p mq now)
        
        merr <- liftIO $ do
          logI $ "got hello from " ++ show pn
          
          atomically $ do
            unless (isJust expected) $ writeTBMQueue mq (Direct $ Hello $ nodeIdentity node)
            writeTBMQueue mq (Direct GetPeerList)
            addPeerNode node pn

        case merr of
            Just err -> error (T.unpack err) -- liftIO $ atomically $ writeTBMQueue mq (Direct $ Bye $ T.unpack err)
            Nothing -> C.addCleanup
              (\_ -> do
                  logI $ "lost connection to " ++ show (peerId $ pnPeer pn)
                  atomically $ removePeerNode node pn)
              (C.mapM_ $ \m -> do
                  getTime >>= atomically . writeTVar (pnLastMessage pn)
                  handlePeerMessages node pn m)
        
      x       -> error $ "unexpected message: " ++ show x
        
  C.addCleanup (\_ -> killThread tid) (C.sourceTBMQueue mq) C.$$ sink

instance (Show a) => UriFetch (Node a) where
  getUriData = requestNodeData

requestNodeData :: (Show a) => Node a -> FN.URI -> IO (Either T.Text (BS.ByteString, Int))
requestNodeData n (FN.CHK loc key extra _) =
  case FN.chkExtraCompression extra of
    Left  e -> return $ Left $ "can't decompress CHK: " `T.append` e
    Right c -> do
      let
        req = FN.ChkRequest loc (FN.chkExtraCrypto extra)
        decrypt blk = case FN.decryptDataBlock blk key $ FN.chkExtraCrypto extra of
          Left e        -> return $ Left $ "decrypting CHK data block failed: " `T.append` e
          Right (p, pl) -> FN.decompressChk c p pl
 
      fromStore <- FN.getChk (nodeFreenet n) req

      case fromStore of
        Right blk -> decrypt blk
        Left _    -> do
          d <- request (nodeChkRequests n) req $ \r -> do
            msg <- mkRoutedMessage n (keyToTarget $ FN.dataRequestLocation req) (FreenetChkRequest r)
            sendRoutedMessage n msg Nothing
          
          result <- atomically $ waitDelayed d
          
          case result of
            Nothing  -> return $ Left "timeout waiting for CHK data"
            Just blk -> decrypt blk
            
requestNodeData n (FN.SSK pkh key extra dn _) = do
  let
    req = FN.SskRequest pkh (FN.sskEncryptDocname key dn) (FN.sskExtraCrypto extra)
    decrypt blk = FN.decryptDataBlock blk key $ FN.sskExtraCrypto extra
        
  fromStore <- FN.getSsk (nodeFreenet n) req

  case fromStore of
    Right blk -> return $ decrypt blk -- (BSL.take (fromIntegral bl) $ BSL.fromStrict blk)
    Left _    -> do
      d <- request (nodeSskRequests n) req $ \r -> do
        msg <- mkRoutedMessage n (keyToTarget $ FN.dataRequestLocation req) (FreenetSskRequest r)
        sendRoutedMessage n msg Nothing
          
      result <- atomically $ waitDelayed d
          
      case result of
        Nothing  -> return $ Left "timeout waiting for SSK data"
        Just blk -> return $ decrypt blk

requestNodeData n (FN.USK pkh key extra dn dr _) = do
  let
    dn' = dn `T.append` "-" `T.append` T.pack (show dr)
    req = FN.SskRequest pkh (FN.sskEncryptDocname key dn') (FN.sskExtraCrypto extra)
    decrypt blk = FN.decryptDataBlock blk key $ FN.sskExtraCrypto extra
        
  fromStore <- FN.getSsk (nodeFreenet n) req

  case fromStore of
    Right blk -> return $ decrypt blk
    Left _    -> do
      d <- request (nodeSskRequests n) req $ \r -> do
        msg <- mkRoutedMessage n (keyToTarget $ FN.dataRequestLocation req) (FreenetSskRequest r)
        sendRoutedMessage n msg Nothing
          
      result <- atomically $ waitDelayed d
          
      case result of
        Nothing  -> return $ Left "timeout waiting for USK data"
        Just blk -> return $ decrypt blk

-------------------------------------------------------------------------------------------------
-- Organizing data requests
-------------------------------------------------------------------------------------------------

data Delayed d = Delayed ! (TMVar (Maybe d))

waitDelayed :: Delayed d -> STM (Maybe d)
waitDelayed (Delayed d) = readTMVar d

data RequestManager r d = RequestManager
                        { rmRequests :: TVar (HMap.HashMap FN.Key (Delayed d))
                        , rmTimeout  :: Int
                        }

mkRequestManager :: STM (RequestManager r d)
mkRequestManager = do
  reqs <- newTVar HMap.empty
  return $! RequestManager reqs (20 * 1000 * 1000)

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
  
