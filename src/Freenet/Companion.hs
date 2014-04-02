
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Companion (
  Companion, initCompanion,

  getChk, getSsk
  ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Monad ( forever, void )
import qualified Data.ByteString as BS
import qualified Data.Configurator as CFG
import qualified Data.Configurator.Types as CFG
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Network
import System.IO

import Freenet.Base64
import Freenet.Chk
import Freenet.Ssk
import Freenet.Types

data Companion = Companion
                 { cHandle :: Handle
                 }

initCompanion :: CFG.Config -> (ChkBlock -> STM ()) -> (SskBlock -> STM ()) -> IO Companion
initCompanion cfg chkHandler sskHandler = do
  host <- CFG.require cfg "host"
  port <- CFG.require cfg "port" :: IO Int

  handle <- connectTo host (PortNumber $ fromIntegral port)
  hSetBuffering handle LineBuffering

  let breakSpace s = let (a, b) = T.break (== ' ') s in (a, T.stripStart b)

  -- parse companion's responses and offer them to Freenet core
  void $ forkIO $ forever $ do
    (what, args) <- breakSpace . decodeUtf8 <$> BS.hGetLine handle
    
    case what of
      "chk" -> do
        let
          (ktxt, rest) = breakSpace args
          (_, rest') = breakSpace rest
          (hstr, rest'') = breakSpace rest'
          (dstr, _) = breakSpace rest''
          df = do
            key <- fromBase64' ktxt >>= mkKey
            hdr <- fromBase64' hstr >>= mkChkHeader
            d <- fromBase64' dstr
            mkChkBlock key hdr d

        case df of
          Left e  -> putStrLn $ "could not parse CHK found response: " ++ T.unpack e
          Right d -> atomically $ chkHandler d

      "ssk" -> do
        let 
          (ktxt, rest)   = breakSpace args
          (pktxt, rest') = breakSpace rest
          (hstr, rest'') = breakSpace rest'
          df = do
            loc    <- fromBase64' ktxt  >>= mkKey
            pubkey <- fromBase64' pktxt >>= mkPubKey
            hdr    <- fromBase64' hstr  >>= mkSskHeader
            d      <- fromBase64' rest''
            mkSskBlock loc hdr d pubkey

        case df of
          Left e  -> putStrLn $ "could not parse SSK found response: " ++ T.unpack e
          Right d -> atomically $ sskHandler d
          
      x -> print $ "strange companion response " ++ T.unpack x
  
  return $ Companion handle

getChk :: Companion -> ChkRequest -> IO ()
getChk comp (ChkRequest k a) =
  let msg = T.intercalate " " ["getchk", toBase64' $ unKey k, T.pack $ show a, "\n"]
  in BS.hPut (cHandle comp) $ encodeUtf8 msg

getSsk :: Companion -> SskRequest -> IO ()
getSsk comp (SskRequest hpk ehd e) = BS.hPut (cHandle comp) $ encodeUtf8 msg where
  msg = T.intercalate " " ["getssk", k $ hpk, k ehd, T.pack $ show e, "\n"]
  k = toBase64' . unKey
