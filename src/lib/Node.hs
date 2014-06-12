
{-# LANGUAGE OverloadedStrings #-}

module Node (
  -- * our node
  Node, mkNode, nodeIdentity, 
  mergeNodeInfo,
  nodeRouteStatus, nodeConnectStatus,
  mkRoutedMessage, peerConnecting
  ) where

import           Control.Applicative ( (<$>) )
import           Control.Concurrent ( forkIO, threadDelay )
import           Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TBMQueue as STM
import           Control.Exception.Base ( finally )
import           Control.Monad ( forever, unless, void, when )

import           Data.Aeson
import qualified Data.Conduit as C
import qualified Data.Conduit.TQueue as C
import qualified Data.HashMap.Strict as HMap
import           Data.List ( minimumBy, nub )
import           Data.Maybe ( isJust )
import qualified Data.Text as T
import           System.IO.Error ( catchIOError )
import           System.Log.Logger
import           System.Timeout ( timeout )

import qualified Freenet.Types as FN
import           Message as MSG
import           Peers
import           Time
import           Types

logD :: String -> IO ()
logD = debugM "node"

logI :: String -> IO ()
logI = infoM "node"

-------------------------------------------------------------------------
-- Node
-------------------------------------------------------------------------

type MessageMap a = TVar (HMap.HashMap MessageId (ActiveMessage a))

data Node a = Node
            { nodePeers       :: TVar [Peer a]     -- ^ the peers we know about
            , nodeConnecting  :: TVar [Peer a]     -- ^ peers we're currently connecting to
            , nodePeerNodes   :: TVar [PeerNode a] -- ^ the nodes we're connected to
            , nodeIdentity    :: TVar (NodeInfo a) -- ^ our identity
            , nodeMidGen      :: MessageIdGen      -- ^ message id generator
            , nodeActMsgs     :: MessageMap a      -- ^ messages we're currently routing
            }

mkNode
  :: PeerAddress a
  => NodeInfo a
  -> IO (Node a)
mkNode self = do
  peers  <- newTVarIO []
  cting  <- newTVarIO []
  pns    <- newTVarIO []
  midgen <- mkMessageIdGen
  msgMap <- newTVarIO HMap.empty
  myId   <- newTVarIO self
  
  let
    node = Node peers cting pns myId midgen msgMap

  void $ forkIO $ maintainConnections node
  return node

readNodeIdentity :: Node a -> STM (NodeInfo a)
readNodeIdentity = readTVar . nodeIdentity

handlePeerMessages :: PeerAddress a => Node a -> PeerNode a -> Message a -> IO ()
handlePeerMessages _ pn (Direct Ping) = logD $ "ping from " ++ show pn

handlePeerMessages node pn (Direct GetPeerList) = atomically $ do
  nis <- readTVar (nodePeers node) >>= mapM mkNodeInfo
  enqMessage pn $ Direct $ PeerList nis

handlePeerMessages node _  (Direct (PeerList ps)) = do
  logD $ "got some peers: " ++ show ps
  atomically $ mapM_ (mergeNodeInfo node) ps
  
handlePeerMessages _ _ _ = return ()
{-
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
-}

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
    myId       <- nodeId <$> readNodeIdentity node
    
    let
      tgt       = toLocation $ (routeTarget msg :: Id)
      nLoc      = peerId . pnPeer
      notMarked = filter (\n -> not $ msg `routeMarked` (nLoc n)) neighbours
      nextBest  = if null notMarked
                  then Nothing
                  else Just $ minimumBy (\n1 n2 -> cmp (toLocation $ nLoc n1) (toLocation $ nLoc n2)) notMarked where
                    cmp l1 l2 = compare d1 d2 where (d1, d2) = (absLocDist tgt l1, absLocDist tgt l2)
      
      msg'      = routeMark msg myId
      mid       = rmId msg
      dropAm    = writeTVar (nodeActMsgs node) $! HMap.delete mid amMap
        
      backtrack = case prev of
        Just p  -> do
          -- a remote message, but we can't make any progress
          enqMessage p (Routed True  msg')
          return $ "immediately backtracked to " ++ show p
          
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
                (p:[]) -> enqMessage p (Routed True  msg') >> dropAm >> (return $ "backtracked to " ++ show p ++ " and dropped")
                (p:ps) -> do
                  enqMessage p (Routed True  msg')
                  writeTVar (nodeActMsgs node) $! HMap.insert mid am { amPreds = ps } amMap
                  return $ "backtracked to " ++ show p
    
    case nextBest of
      Nothing   -> backtrack
      Just next ->
        let
          -- update or create the AM
          am' = case HMap.lookup mid amMap of
            Nothing -> ActiveMessage now [] (rmPayload msg) next
            Just am -> am { amStarted = now, amLastForwarded = next }

          -- add predecessor
          am'' = case prev of
            Nothing -> am'
            Just p  -> am' { amPreds = (p : amPreds am') }
          
          forward m = do
            writeTVar (nodeActMsgs node) $! HMap.insert mid am'' amMap
            enqMessage next (Routed False m)
            return $ "forwarded to " ++ show next

          nd = absLocDist (toLocation $ nLoc next) tgt -- dist (nextNode, t)
          md = absLocDist (toLocation myId) tgt        -- dist (v, t)
          
        in if nd >= md
           then forward msg' -- forward and mark
           else forward msg  -- forward but don't mark

  logD $ "routed message " ++ show (rmId msg) ++ ": " ++ logMsg
    
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

-- |
-- Merges the information about some peer with our set of known peers,
-- adding new ones or just updating addresses of the ones we already
-- know about.
mergeNodeInfo :: PeerAddress a => Node a -> NodeInfo a -> STM ()
mergeNodeInfo node ni = readNodeIdentity node >>= \myid -> unless (nodeId ni == nodeId myid) $ do
  p <- mkPeer ni

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

maintainConnections :: PeerAddress a => Node a -> IO ()
maintainConnections node = forever $ do
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

  void $ forkIO $ connectPeer shouldConnect $ \cresult -> do
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

-- |
-- Offers an incoming connection from a peer to our node to deal with. This
-- will be ultimately called upon an incoming TCP connection or similar.
peerConnecting
  :: PeerAddress a
  => Node a        -- ^ our node
  -> MessageIO a   -- ^ the @MessageIO@ interface of the connecting peer
  -> IO ()
peerConnecting node io = runPeerNode node io Nothing

-- |
-- handle a node that is currently connected to us
runPeerNode
  :: PeerAddress a
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
              myid <- readNodeIdentity node
              unless (isJust expected) (enqDirect $ Hello myid)
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
  when (isJust expected) $ atomically $ 
    readNodeIdentity node >>= enqDirect . Hello
  
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

