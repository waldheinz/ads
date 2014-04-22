
module Time (
  Timestamp, getTime,
  Timediff, timeDiff, timeDiffSeconds
  ) where

import Data.Aeson
import Data.Time.Clock.POSIX ( getPOSIXTime )

newtype Timestamp = Timestamp { unTs :: Double } deriving ( Show )

getTime :: IO Timestamp
getTime = (Timestamp . realToFrac) `fmap` getPOSIXTime

newtype Timediff = Timediff { timeDiffSeconds :: Double } deriving ( Show )

timeDiff :: Timestamp -> Timestamp -> Timediff
timeDiff ts1 ts2 = Timediff $ (unTs ts2) - (unTs ts1)

instance ToJSON Timediff where
  toJSON td = toJSON $ timeDiffSeconds td
