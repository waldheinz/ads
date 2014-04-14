
module Time (
  Timestamp, getTime
  ) where

import Data.Time.Clock.POSIX ( getPOSIXTime )

newtype Timestamp = Timestamp { _unTimeStamp :: Double }

getTime :: IO Timestamp
getTime = (Timestamp . realToFrac) `fmap` getPOSIXTime
