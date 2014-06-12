
module Logging (
  initLogging,

  module System.Log.Logger

  ) where

--import qualified Data.Configurator as CFG
--import qualified Data.Configurator.Types as CFG
import System.IO ( stderr )
import System.Log.Handler.Simple
import System.Log.Logger

-- | configure log handlers
initLogging :: IO ()
initLogging = do
  h <- verboseStreamHandler stderr DEBUG
  updateGlobalLogger rootLoggerName $ setHandlers [h]
  updateGlobalLogger rootLoggerName $ setLevel INFO
