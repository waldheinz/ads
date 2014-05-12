
{-# LANGUAGE OverloadedStrings #-}

module Node (
  -- * our node
  Node, mkNode, readPeers,
  requestNodeData, nodeArchives,
  nodeFreenet,

  nodeRouteStatus, nodeConnectStatus,
  
  -- * peers
  ConnectFunction,
  
  -- * other nodes we're connected to
  PeerNode, runPeerNode,

  -- * fetching data
  nodeFetchChk, nodeFetchSsk
  ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBMQueue as STM
import Control.Exception.Base ( finally ) 
import Control.Monad ( forever, unless, void, when )

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import qualified Data.Conduit.TQueue as C
import Data.List ( minimumBy, nub )
import qualified Data.HashMap.Strict as HMap
import Data.Maybe ( isJust )
import qualified Data.Text as T
import System.FilePath ( (</>) )
import System.IO.Error ( catchIOError )
import System.Log.Logger
import System.Timeout ( timeout )

import qualified Freenet as FN
import qualified Freenet.Archive as FN
import qualified Freenet.Chk as FN
import qualified Freenet.Ssk as FN
import qualified Freenet.Types as FN
import qualified Freenet.URI as FN
import Message as MSG
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
            { nodePeers       :: TVar [Peer a]                                   -- ^ the peers we know about
            , nodeConnecting  :: TVar [Peer a]                                   -- ^ peers we're currently connecting to
            , nodePeerNodes   :: TVar [PeerNode a]                               -- ^ the nodes we're connected to
            , nodeIdentity    :: NodeInfo a                                      -- ^ our identity
            , nodeMidGen      :: MessageIdGen                                    -- ^ message id generator
            , nodeActMsgs     :: TVar (HMap.HashMap MessageId (ActiveMessage a)) -- ^ messages we're currently routing
            , nodeFreenet     :: FN.Freenet a                                    -- ^ our freenet compatibility layer
            , nodeArchives    :: FN.ArchiveCache                                 -- ^ Freenet LRU archive cache
            , nodeChkRequests :: RequestManager FN.ChkRequest FN.ChkBlock
            , nodeSskRequests :: RequestManager FN.SskRequest FN.SskBlock
            }

mkNode :: PeerAddress a => NodeInfo a -> FN.Freenet a -> ConnectFunction a -> IO (Node a)
mkNode self fn connect = do
  peers  <- newTVarIO []
  connecting <- newTVarIO [] -- peers we're currently connecting to
  pns    <- newTVarIO []
  midgen <- mkMessageIdGen
  msgMap <- newTVarIO HMap.empty
  ac     <- FN.mkArchiveCache 10
  chkRm  <- atomically mkRequestManager
  sskRm  <- atomically mkRequestManager

  let
    node = Node peers connecting pns self midgen msgMap fn ac chkRm sskRm

  void $ forkIO $ maintainConnections node connect
  return node

handlePeerMessages :: PeerAddress a => Node a -> PeerNode a -> Message a -> IO ()
handlePeerMessages _ pn (Direct Ping) = logD $ "ping from " ++ show pn

handlePeerMessages node pn (Direct GetPeerList) = atomically $ do
  nis <- readTVar (nodePeers node) >>= mapM mkNodeInfo
  enqMessage pn $ Direct $ PeerList nis

handlePeerMessages node _  (Direct (PeerList ps)) = do
  logD $ "got some peers: " ++ show ps
  atomically $ mapM_ (mergeNodeInfo node) ps
  
handlePeerMessages node pn msg = do
  let
    fn = nodeFreenet node
    peer = pnPeer pn
    
    route = void $ forkIO $ case msg of
      Routed False rm@(RoutedMessage (FreenetChkRequest req) mid _ _) -> do
        local <- FN.getChk fn req
        case local of
          Left _    -> sendRoutedMessage node rm (Just pn) -- pass on
          Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetChkBlock blk

      Routed False rm@(RoutedMessage (FreenetSskRequest req) mid _ _) -> do
        local <- FN.getSsk fn req
        case local of
          Left _    -> sendRoutedMessage node rm (Just pn) -- pass on
          Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetSskBlock blk
          
      Routed True rm    -> do
        -- the message was backtracked to us
        case rmPayload rm of
          (FreenetChkRequest req) -> atomically $ peerFetchDone peer (FN.dataRequestLocation req) False
          _                       -> return ()
          
        sendRoutedMessage node rm Nothing
        
      Response mid msg' -> (atomically $ handleResponse node peer mid msg') >>=
                            \lm -> logD $ "routed " ++ show mid ++ ": " ++ show lm

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
                       { amStarted       :: ! Timestamp          -- ^ when this message was last routed
                       , amPreds         :: ! [PeerNode a]       -- ^ predecessors (peers who think we're close to the target)
                       , amPayload       :: ! (MessagePayload a) -- ^ payload of that message
                       , amLastForwarded :: ! (PeerNode a)       -- ^ where the message was last forwarded
                       }

instance Show a => Show (ActiveMessage a) where
  show (ActiveMessage s ps _ _) = "ActiveMessage {amStarted=" ++ show s ++ ", amPreds=" ++ show ps ++ "}"

handleResponse :: Show a => Node a -> Peer a -> MessageId -> MessagePayload a -> STM String
handleResponse node peer mid msg = do
  m <- readTVar (nodeActMsgs node)
  
  case HMap.lookup mid m of
    Nothing -> return "no active message with this id"
    Just am -> do
      (m', logMsg) <- case amPreds am of
        []      -> do
          -- this is the last time we see this AM, we can update the stats and drop it
          case msg of
            FreenetChkBlock blk -> peerFetchDone peer (FN.dataBlockLocation blk) True
            _                   -> return ()
            
          return (HMap.delete mid m, "no preds")
        (pn:[]) -> enqMessage pn (Response mid msg) >> return (HMap.delete mid m, "sent and dropped AM")
        (pn:ps) -> enqMessage pn (Response mid msg) >> return (HMap.insert mid (am { amPreds = ps }) m, "sent to " ++ show pn)
      
      seq m' $ writeTVar (nodeActMsgs node) m' >> return logMsg
      
mkRoutedMessage :: (HasId i, PeerAddress a) => Node a -> i -> MessagePayload a -> IO ()
mkRoutedMessage node target msg = do
  mid <- atomically $ nextMessageId $ nodeMidGen node
  sendRoutedMessage node (RoutedMessage msg mid [] (getId target)) Nothing

sendRoutedMessage :: PeerAddress a => Node a -> RoutedMessage a -> Maybe (PeerNode a) -> IO ()
sendRoutedMessage node msg prev = do
  now <- getTime
  
  logMsg <- atomically $ do
    amMap      <- readTVar $ nodeActMsgs node
    neighbours <- readTVar $ nodePeerNodes node

    let
      myId      = nodeId $ nodeIdentity node
      tgt       = toLocation $ (routeTarget msg :: Id)
      nLoc      = peerId . pnPeer
      notMarked = filter (\n -> not $ msg `routeMarked` (nLoc n)) neighbours
      next      = minimumBy (\n1 n2 -> cmp (toLocation $ nLoc n1) (toLocation $ nLoc n2)) notMarked where
        cmp l1 l2 = compare d1 d2 where (d1, d2) = (absLocDist tgt l1, absLocDist tgt l2)
      msg'      = routeMark msg myId
      mid       = rmId msg
      dropAm    = writeTVar (nodeActMsgs node) $! HMap.delete mid amMap
      
      forward m = do
        let
          -- update or create the AM
          am' = case HMap.lookup mid amMap of
            Nothing -> ActiveMessage now [] (rmPayload msg) next
            Just am -> am { amStarted = now, amLastForwarded = next }

          -- add predecessor
          am'' = case prev of
            Nothing -> am'
            Just p  -> am' { amPreds = (p : amPreds am') }
            
        writeTVar (nodeActMsgs node) $! HMap.insert mid am'' amMap
        enqMessage next (Routed False m)
        return $ "forwarded to " ++ show next
        
      backtrack = case prev of
        Just p  -> do
          -- a remote message, but we can't make any progress
          enqMessage p (Routed True  msg')
          dropAm
          return "immediately backtracked"
          
        Nothing -> do
          -- the message was either local or backtracked to us
          case HMap.lookup mid amMap of
            Nothing -> return "unknown message"
            Just am -> do
              -- whoever we sent the message to did not make any progress, update stats
              case rmPayload msg of
                FreenetChkRequest req -> peerFetchDone (pnPeer $ amLastForwarded am) (FN.dataRequestLocation req) False
                _                     -> return ()
              
              -- try to pop a predecessor
              case amPreds am of
                []     -> dropAm >> return "routing failed"
                (p:ps) -> do
                  enqMessage p (Routed True  msg')
                  if null ps
                    then writeTVar (nodeActMsgs node) $! HMap.delete mid amMap
                    else writeTVar (nodeActMsgs node) $! HMap.insert mid am { amPreds = ps } amMap
                  return $ "backtracked to " ++ show p
              
    if (not . null) notMarked
      then if absLocDist (toLocation $ nLoc next) tgt >= absLocDist (toLocation myId) tgt
           then forward msg  -- forward but don't mark
           else forward msg' -- forward and mark
      else backtrack -- check if we can backtrack
      
{-    
    let
      mm = mark msg $ selfLocation v

    s <- filter (\n -> not $ marked m (neighbourLocation v n)) <$> neighbours v
  
    if (not . null) s
      then do
        let next = closest s
                 
        if absLocDist (toLocation $ neighbourLocation v next) tgt >= absLocDist (toLocation $ selfLocation v) tgt
          then return $ Forward next mm
          else return $ Forward next msg
      else do
      -- check if we can backtrack
      
        p <- popPred v msg
        case p of
          Nothing -> return Fail
          Just pp -> return $ Backtrack pp mm
    
    -- create an ActiveMessage or update the message's timestamp
    let
      mid = rmId msg
      go Nothing   = ActiveMessage now [] (rmPayload msg)
      go (Just am) = am { amStarted = now }
      in modifyTVar' (nodeActMsgs node) $ \m -> HMap.insert mid (go $ HMap.lookup mid m) m
  
    -- let the routing run
    NBO.route (nodeNbo node) prev msg >>= \nextStep -> case nextStep of
      NBO.Forward dest msg'   -> enqMessage dest (Routed False msg') >> (return $ "forwarded to " ++ show dest)
      NBO.Backtrack dest msg' -> enqMessage dest (Routed True  msg') >> (return $ "backtracked to " ++ show dest)
      NBO.Fail                -> case prev of
        Nothing -> do
          modifyTVar' (nodeActMsgs node) $ HMap.delete (rmId msg)
          return "routing failed"
            
        Just pn -> handleResponse node (pnPeer pn) (rmId msg) (Failed $ Just "routing failed")
-}
  logD $ "routed message " ++ show (rmId msg) ++ ": " ++ logMsg
  
{-
messagePushPred :: Node a -> MessageId -> PeerNode a -> STM ()
messagePushPred node mid pn = modifyTVar' (nodeActMsgs node) $ HMap.adjust prepend mid
  where
    prepend am = am { amPreds = (pn : amPreds am) }

messagePopPred :: Node a -> MessageId -> STM (Maybe (PeerNode a))
messagePopPred node mid = do
  let
    amMap = nodeActMsgs node
  
  m <- readTVar amMap
  
  let
    tgt = HMap.lookup mid m
    
  writeTVar amMap $ case tgt of
      Just (ActiveMessage _ (_:[]) _) -> HMap.delete mid m -- remote messages without any more preds can be dropped
      Just (ActiveMessage s (_:xs) p) -> HMap.insert mid (ActiveMessage s xs p) m -- messages with more preds just get a pred removed
      _                               -> m
  
  case tgt of
    Just (ActiveMessage _ (x:_) _) -> return $ Just x
    _                              -> return Nothing
-}

-- |
-- Generate JSON containing some information about the currently
-- routed messages.
nodeRouteStatus :: Node a -> IO Value
nodeRouteStatus node = do
  now <- getTime
  
  let
    toState xs mid am = x : xs where
      x = object
          [ "messageId" .= mid
          , "age"       .= timeDiff (amStarted am) now
          , "preds"     .= map (peerId . pnPeer) (amPreds am)
          ]
  
  msgs <- HMap.foldlWithKey' toState [] <$> atomically (readTVar $ nodeActMsgs node)
  
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
  kpbs <- catchIOError
          (eitherDecode <$> BSL.readFile kpFile)
          (\e -> return $ Left $ show e)
  
  case kpbs of
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
    merge (Peer _ av1 _) (Peer _ av2 _) = do
      as2 <- readTVar av2
      modifyTVar' av1 $ \as1 -> nub (as1 ++ as2)
  
    go [] = return [p]
    go (x:xs)
      | x == p = merge x p >> return (x:xs)
      | otherwise = go xs >>= \xs' -> return (x:xs')
  
  readTVar (nodePeers node) >>= go >>= writeTVar (nodePeers node)
  
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
-- Remove a peer from the set of connected peers. We decided
-- we don't want to talk to this peer any more, or the connection
-- was lost.
removePeerNode :: Node a -> PeerNode a -> STM ()
removePeerNode node pn = do
  modifyTVar' (nodePeerNodes node) $ filter (/= pn)     -- no longer connected
  modifyTVar' (nodeActMsgs node)   $ HMap.map dropPreds -- and we can drop messages routed for this node
  where
    dropPreds am = am { amPreds = filter (/= pn) (amPreds am) } -- TODO: some AMs should be re-sent to other peers

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
      result = [x | x <- known, x `notElem` cting, x `notElem` cpeers]

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
    case cresult of
      Left _      -> atomically $ modifyTVar' (nodeConnecting node) (filter ((/=) shouldConnect))
      Right msgio -> runPeerNode node msgio (Just $ peerId shouldConnect)

-------------------------------------------------------------------------
-- Peer Nodes
-------------------------------------------------------------------------

-- |
-- A @PeerNode@ is a @Peer@ we're currently connected to.
data PeerNode a
  = PeerNode
    { pnPeer      :: Peer a               -- ^ the peer
    , pnQueue     :: TBMQueue (Message a) -- ^ outgoing message queue
    , pnConnected :: Timestamp            -- ^ when this peer connected
    }
    
instance (Show a) => Show (PeerNode a) where
  show n = "Node {peer = " ++ show (peerId $ pnPeer n) ++ " }"

instance Eq (PeerNode a) where
  (PeerNode p1 _ _) == (PeerNode p2 _ _) = p1 == p2

--instance HasLocation (PeerNode a) where
--  toLocation = toLocation . peerId . pnPeer 

nodeConnectStatus :: ToJSON a => Node a -> IO Value
nodeConnectStatus node = do
  now <- getTime
  let 
      pnJSON pn = do
        p   <- toStateJSON $ pnPeer pn
        oqs <- freeSlotsTBMQueue $ pnQueue pn
           
        return $ object
                   [ "peer"         .= p
                   , "outQueueFree" .= oqs
                   , "connectedFor" .= timeDiff (pnConnected pn) now
                   ]
        
  cs <- atomically $ readTVar (nodePeerNodes node) >>= mapM pnJSON
  return $ object [ "connected" .= cs ]

-- | puts a message on the Node's outgoing message queue       
enqMessage :: PeerNode a -> Message a -> STM ()
enqMessage n = writeTBMQueue (pnQueue n)

------------------------------------------------------------------------------
-- Handshake
------------------------------------------------------------------------------

-- ^ handle a node that is currently connected to us
runPeerNode
  :: (PeerAddress a)
  => Node a
  -> MessageIO a    -- ^ the (source, sink) pair to talk to the peer
  -> Maybe Id
  -> IO ()
runPeerNode node (src, sink) expected = do
  outq <- newTBMQueueIO 10
  inq  <- newTBMQueueIO 10
  now  <- getTime
  
  let
    enqDirect msg = writeTBMQueue outq $ Direct msg
    
    byeError reason = do
      logI $ "dropping connection: " ++ T.unpack reason
      atomically $ enqDirect (Bye $ T.unpack reason) >> closeTBMQueue outq
    
    checkId pn = case expected of
      Nothing -> Nothing
      Just e  -> if e /= nid then Just "node identity mismatch" else Nothing
      where
        nid = peerId $ pnPeer pn
        
    mkPn ninfo = do
      p <- mkPeer ninfo
      return $ PeerNode p outq now

    -- inject a ping every 30s to prevent timeout on the other side
    sendPings = do
      to <- registerDelay $ 30 * 1000 * 1000
      closed <- atomically $ readTVar to >>= \t -> if not t then retry else do
        c <- isClosedTBMQueue outq
        if c
          then return True
          else do
            f <- isFullTBMQueue outq
            unless f $ writeTBMQueue outq (Direct Ping)
            return False

      unless closed sendPings
    
    handshake ni = atomically $ do
        pn <- mkPn ni
        
        case checkId pn of
          Just e  -> return $ Left e
          Nothing -> addPeerNode node pn >>= \ea -> case ea of
            Just e' -> return $ Left e'
            Nothing -> do
              unless (isJust expected) (enqDirect $ Hello $ nodeIdentity node)
              enqDirect GetPeerList
              return $ Right pn

    handleMessages pn = do
      timeo <- registerDelay $ 60 * 1000 * 1000 -- we want some message at least every minute
      msg   <- atomically $ orElse
               (readTVar timeo   >>= \to -> if to then return (Left "timeout waiting for message") else retry)
               (readTBMQueue inq >>= \mmsg -> case mmsg of
                   Nothing -> return $ Left "connecting closed waiting for message"
                   Just m  -> return $ Right m)
  
      case msg of
        Left err -> byeError err
        Right m  -> handlePeerMessages node pn m >> handleMessages pn

  void $ forkIO $ catchIOError
    (src C.$$ (C.sinkTBMQueue inq True))
    (\_ -> atomically $ closeTBMQueue inq >> closeTBMQueue outq)
    
  void $ forkIO $ C.addCleanup
    (\_ -> atomically $ closeTBMQueue inq)
    (C.sourceTBMQueue outq) C.$$ sink

  -- enqueue the obligatory Hello message, if this is an outgoing connection
  when (isJust expected) $ atomically . enqDirect $ Hello $ nodeIdentity node
  
  -- wait for peer's hello (or timeout)
  mnode <- timeout (5 * 1000 * 1000) $ do
    msg <- atomically $ readTBMQueue inq
    
    -- we'd like to see a hello message, everything else indicates failure
    case msg of
      Just (Direct (Hello ni)) -> handshake ni
      Nothing                  -> return $ Left $ "connection closed during handshake"
      Just (Direct (Bye r))    -> return $ Left $ T.pack $ "connection rejected: " ++ r
      Just x                   -> return $ Left $ T.pack $ "unexpected message " ++ show x

  -- now the peer has either been promoted to a PeerNode (in handshake), or it will
  -- never be. anyway, it's safe to say we've left "connecting" state for this one
  case expected of
    Nothing -> return ()
    Just x  -> atomically $ modifyTVar' (nodeConnecting node) (filter (\p -> peerId p /= x))
  
  case mnode of
    Nothing         -> logI $ "timeout waiting for peer hello"
    Just (Left e)   -> logI $ "handshake failed: " ++ T.unpack e
    Just (Right pn) -> do
      logI $ "finished handshake with " ++ show pn
      void $ forkIO $ sendPings
      finally (handleMessages pn) (atomically $ removePeerNode node pn)

  mapM_ (atomically . closeTBMQueue) [inq, outq]
  
  -- let outgoing queue flush
  threadDelay $ 1000 * 1000

------------------------------------------------------------------------------------------
-- fetching data
------------------------------------------------------------------------------------------

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
                    
instance PeerAddress a => FN.UriFetch (Node a) where
  getUriData = requestNodeData

instance PeerAddress a => FN.ChkInsert (Node a) where
  insertChk node chk = FN.offerChk (nodeFreenet node) chk

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
  
