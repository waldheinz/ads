
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
import Data.List ( nub )
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
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
import qualified NextBestOnce as NBO
import Peers
import Time
import Types

import Debug.Trace

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
handlePeerMessages _ pn (Direct Ping) = logD $ "ping from " ++ show pn

handlePeerMessages node pn (Direct GetPeerList) = atomically $ do
  nis <- readTVar (nodePeers node) >>= mapM mkNodeInfo
  enqMessage pn $ Direct $ PeerList nis

handlePeerMessages node _  (Direct (PeerList ps)) = do
  logD $ "got some peers: " ++ show ps
  atomically $ mapM_ (mergeNodeInfo node) ps
  
handlePeerMessages node pn msg = do
  logI $ "handling " ++ show msg
  
  let
    fn = nodeFreenet node
    
    route = void $ forkIO $ do
     case msg of
      Routed False rm@(RoutedMessage (FreenetChkRequest req) mid _) -> do
        local <- FN.getChk fn req
        case local of
          Left _    -> sendRoutedMessage node rm (Just pn) Nothing -- (forwardResponse node mid)  -- pass on
          Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetChkBlock blk

      Routed False rm@(RoutedMessage (FreenetSskRequest req) mid _) -> do
        local <- FN.getSsk fn req
        case local of
          Left _    -> sendRoutedMessage node rm (Just pn) Nothing -- (forwardResponse node mid) -- pass on
          Right blk -> atomically $ enqMessage pn $ Response mid $ FreenetSskBlock blk
          
      Routed True rm    -> sendRoutedMessage node rm Nothing Nothing -- (const $ return "backtrack") -- backtrack
      Response mid msg' -> (atomically $ handleResponse node mid msg') >>= \log -> logI $ "routed " ++ show mid ++ ": " ++ show log

      _ -> return ()
      
     rs <- nodeRouteStatus node
     logI $ "routing state after message handling: " ++ (show $ toJSON rs)
  
    writeStores = case msg of
      Response _ (FreenetChkBlock blk) -> do
        atomically $ offer blk (nodeChkRequests node)
        FN.offerChk (nodeFreenet node) blk
        
      Response _ (FreenetSskBlock blk) -> do
        atomically $ offer blk (nodeSskRequests node)
        FN.offerSsk (nodeFreenet node) blk
      _   -> return ()
    
  writeStores >> route


forwardResponse :: PeerAddress a => Node a -> MessageId -> MessagePayload a -> STM String
forwardResponse node mid msg = do
  mtgt <- messagePopPred node mid
  
  case mtgt of
    Nothing -> return $ "could not send response, message id unknown: " ++ show mid
    Just pn -> enqMessage pn (Response mid msg) >> (return $ "forwarded to " ++ show pn)

handleResponse :: Show a => Node a -> MessageId -> MessagePayload a -> STM String
handleResponse node mid msg = do
  m <- readTVar (nodeActMsgs node)
    
  case Map.lookup mid m of
    Nothing -> return "no active message with this id"
    Just am -> do
      -- notify local handler immediately, but only once
      am' <- case amResponse am of
        Nothing -> return am
        Just h  -> h msg >> return am { amResponse = Nothing }

      (m', logMsg) <- case amPreds am' of
        []      -> return (Map.delete mid m, "no preds") -- if there are no preds, we can drop the AM
        (pn:ps) -> enqMessage pn (Response mid msg) >> return (Map.insert mid (am' { amPreds = ps }) m, "sent to " ++ show pn)

      writeTVar (nodeActMsgs node) m' >> return logMsg
      
-----------------------------------------------------------------------------------------------
-- Routing
-----------------------------------------------------------------------------------------------

type ResponseHandler a = MessagePayload a -> STM String -- give back a log message

data ActiveMessage a = ActiveMessage
                       { amStarted  :: Timestamp                 -- ^ when this message was sent off
                       , amPreds    :: [PeerNode a]              -- ^ predecessors (peers who think we're close to the target)
                       , amResponse :: Maybe (ResponseHandler a) -- ^ what to do when the response arrives, only for locally generated messages
                       }

instance Show a => Show (ActiveMessage a) where
  show (ActiveMessage s ps _) = "ActiveMessage {amStarted=" ++ show s ++ ", amPreds=" ++ show ps ++ "}"

mkRoutedMessage :: PeerAddress a => Node a -> NodeId -> MessagePayload a -> ResponseHandler a -> IO ()
mkRoutedMessage node target msg onResponse = do
  mid <- atomically $ nextMessageId $ nodeMidGen node
  sendRoutedMessage node (RoutedMessage msg mid $ NBO.mkRoutingInfo target) Nothing (Just onResponse)

sendRoutedMessage :: PeerAddress a => Node a -> RoutedMessage a -> Maybe (PeerNode a) -> Maybe (ResponseHandler a) -> IO ()
sendRoutedMessage node msg prev onResp = do
  now <- getTime
  
  logMsg <- atomically $ do
    -- create an ActiveMessage or update the message's timestamp
    let
      go Nothing   = Just $ ActiveMessage now [] onResp
      go (Just am) = Just $ am { amStarted = now {- , amResponse = rh-} }
      in modifyTVar' (nodeActMsgs node) $ Map.alter go (rmId msg)
  
    -- let the routing run
    tmpmap <- readTVar (nodeActMsgs node)
    
    traceShow tmpmap $ NBO.route (nodeNbo node) prev msg >>= \nextStep -> traceShow ("nextStep", nextStep) $ case nextStep of
      NBO.Forward dest msg'   -> enqMessage dest (Routed False msg') >> (return $ "forwarded to " ++ show dest)
      NBO.Backtrack dest msg' -> enqMessage dest (Routed True  msg') >> (return $ "backtracked to " ++ show dest)
      NBO.Fail                -> handleResponse node (rmId msg) (Failed $ Just "routing failed")
  
  logI $ "routed message " ++ show (rmId msg) ++ ": " ++ logMsg

messagePushPred :: Node a -> MessageId -> PeerNode a -> STM ()
messagePushPred node mid pn = modifyTVar' (nodeActMsgs node) $ Map.update prepend mid
  where
    prepend am = Just $ am { amPreds = (pn : amPreds am) }

messagePopPred :: Node a -> MessageId -> STM (Maybe (PeerNode a))
messagePopPred node mid = do
  let
    amMap = nodeActMsgs node
  
  m <- readTVar amMap
  
  let
    tgt = Map.lookup mid m
    m'  = Map.update pop mid m
    pop am = Just $ case amPreds am of
      (_:xs) -> am { amPreds = xs }
      _      -> am
    
  writeTVar amMap m'
  
  case tgt of
    Just (ActiveMessage _ (x:_) _) -> return $ Just x
    _                              -> return Nothing


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
-- Removed a peer from the set of connected peers, when we decided
-- we don't want to talk to this peer any more, or the connection
-- was lost.
removePeerNode :: Node a -> PeerNode a -> STM ()
removePeerNode node pn = do
  modifyTVar' (nodePeerNodes node) $ filter (/= pn)         -- it's no longer connected
  modifyTVar' (nodeActMsgs node)   $ Map.mapMaybe dropPreds -- and we can drop messages routed for this node
  where
    dropPreds am = Just am { amPreds = filter (/= pn) (amPreds am) }

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
    { pnPeer        :: Peer a               -- ^ the peer
    , pnQueue       :: TBMQueue (Message a) -- ^ outgoing message queue
--    , pnLastMessage :: TVar Timestamp       -- ^ when the last message was received
    }
    
instance (Show a) => Show (PeerNode a) where
  show n = "Node {peer = " ++ show (peerId $ pnPeer n) ++ " }"

instance Eq (PeerNode a) where
  (PeerNode p1 _) == (PeerNode p2 _) = p1 == p2

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

------------------------------------------------------------------------------
-- Handshake
------------------------------------------------------------------------------

-- ^ handle a node that is currently connected to us
runPeerNode
  :: (PeerAddress a)
  => Node a
  -> MessageIO a    -- ^ the (source, sink) pair to talk to the peer
  -> Maybe NodeId
  -> IO ()
runPeerNode node (src, sink) expected = do
  outq <- newTBMQueueIO 10
  inq  <- newTBMQueueIO 10

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
      return $ PeerNode p outq

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
{-          
        (isClosedTBMQueue outq >>= \c -> if c then return True else retry) `orElse`
        (isFullTBMQueue outq  >>= \f -> if f then return False else retry) `orElse`
        (writeTBMQueue outq (Direct Ping)
  -}              
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
  
instance PeerAddress a => UriFetch (Node a) where
  getUriData = requestNodeData

requestNodeData :: PeerAddress a => Node a -> FN.URI -> IO (Either T.Text (BS.ByteString, Int))
requestNodeData n (FN.CHK loc key extra _) =
  case FN.chkExtraCompression extra of
    Left  e -> return $ Left $ "can't decompress CHK: " `T.append` e
    Right c -> do
      let
        req = FN.ChkRequest loc (FN.chkExtraCrypto extra)
        decrypt blk = case FN.decryptDataBlock blk key of
          Left e        -> return $ Left $ "decrypting CHK data block failed: " `T.append` e
          Right (p, pl) -> FN.decompressChk c p pl
 
      fromStore <- FN.getChk (nodeFreenet n) req

      case fromStore of
        Right blk -> decrypt blk
        Left _    -> do
          d <- request (nodeChkRequests n) req $ \r -> do
            mkRoutedMessage n (keyToNodeId $ FN.dataRequestLocation req) (FreenetChkRequest r)
              (\repl -> return $ "chk reply " ++ show repl)
          
          result <- atomically $ waitDelayed d
          
          case result of
            Nothing  -> return $ Left "timeout waiting for CHK data"
            Just blk -> decrypt blk
            
requestNodeData n (FN.SSK pkh key extra dn _) = do
  let
    req = FN.SskRequest pkh (FN.sskEncryptDocname key dn) (FN.sskExtraCrypto extra)
    decrypt blk = FN.decryptDataBlock blk key
        
  fromStore <- FN.getSsk (nodeFreenet n) req

  case fromStore of
    Right blk -> return $ decrypt blk -- (BSL.take (fromIntegral bl) $ BSL.fromStrict blk)
    Left _    -> do
      d <- request (nodeSskRequests n) req $ \r -> do
        mkRoutedMessage n (keyToNodeId $ FN.dataRequestLocation req) (FreenetSskRequest r)
          (const $ return $ "ssk reply " ++ show req)
          
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
          (keyToNodeId $ FN.dataRequestLocation req)
          (FreenetSskRequest r)
          (\repl -> return $ "usk reply" ++ show repl)
          
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
  
