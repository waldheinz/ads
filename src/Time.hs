
module Time (
  Timestamp, getTime,
  Timediff, timeDiff
  ) where

import Data.Aeson
import Data.Time.Clock.POSIX ( getPOSIXTime )

newtype Timestamp = Timestamp { unTs :: Double }

getTime :: IO Timestamp
getTime = (Timestamp . realToFrac) `fmap` getPOSIXTime

newtype Timediff = Timediff { unTd :: Double }

timeDiff :: Timestamp -> Timestamp -> Timediff
timeDiff ts1 ts2 = Timediff $ (unTs ts2) - (unTs ts1)

instance ToJSON Timediff where
  toJSON td = toJSON $ unTd td
