module PACT.Capability.Log
  ( module PACT.Data.Log
  , class Log
  , logMessage
  , log
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logHush
  , debugHush
  ) where

import Prelude (class Monad, Unit, pure, ($), (*>), (<<<), (<=<), (>>=))
import PACT.Capability.Now (class Now)
import PACT.Data.Log (LogEntry, LogType(..), logContents, logType, mkLogEntry)
import Halogen (HalogenM)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Trans.Class (lift)

class
  (Monad m, Now m) <= Log m where
  logMessage :: LogEntry -> m Unit

instance Log m => Log (HalogenM st act slots out m) where
  logMessage = lift <<< logMessage

log :: forall m. Log m => LogType -> String -> m Unit
log logT = logMessage <=< mkLogEntry logT

-- | Log a message for debugging purposes
logDebug :: forall m. Log m => Now m => String -> m Unit
logDebug = log Debug

-- | Log a message to convey non-error information
logInfo :: forall m. Log m => Now m => String -> m Unit
logInfo = log Info

-- | Log a message as a warning
logWarn :: forall m. Log m => Now m => String -> m Unit
logWarn = log Warn

-- | Log a message as an error
logError :: forall m. Log m => Now m => String -> m Unit
logError = log Error

-- | Hush a monadic action by logging the error, leaving it open why the error
-- | is being logged
logHush :: forall m a. Log m => Now m => LogType -> m (Either String a) -> m (Maybe a)
logHush reason action =
  action
    >>= case _ of
        Left e -> case reason of
          Debug -> logDebug e *> pure Nothing
          Info -> logInfo e *> pure Nothing
          Warn -> logWarn e *> pure Nothing
          Error -> logError e *> pure Nothing
        Right v -> pure $ Just v

-- | Hush a monadic action by logging the error in debug mode
debugHush :: forall m a. Log m => Now m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug
