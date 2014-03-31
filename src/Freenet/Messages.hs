
-- |
-- Messages Freenet exchanges with other nodes.
module Freenet.Messages (
  FreenetMessage(..)
  ) where

import Control.Applicative ( (<$>) )
import Data.Binary

import Freenet.Types

data FreenetMessage
     = DataRequestMsg DataRequest
     deriving ( Show )

instance Binary FreenetMessage where
  put (DataRequestMsg dr) = put dr
  get = DataRequestMsg <$> get
  
