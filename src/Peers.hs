
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Maintain connections to other peers.
module Peers (
  ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.TQueue as C

import Message
import Types
