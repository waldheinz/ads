
-- |
-- Messages Freenet exchanges with other nodes.
module Freenet.Messages (
  Message(..)
  ) where

import Freenet.Types

data Message
     = DataRequestMsg DataRequest
     deriving ( Show )
