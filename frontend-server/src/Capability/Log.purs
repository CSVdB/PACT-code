-- TODO: Separate the data types from the (monad) functionality?
module PACT.Capability.Log
  ( LogType(..)
  , logType
  , logContents
  , class Log
  , logMessage
  , log
  , LogEntry
  , mkLogEntry
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logHush
  , debugHush
  ) where

import Prelude
import Halogen (HalogenM)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (formatDateTime)
import PACT.Capability.Now (class Now, nowDateTime)
import Data.Maybe (Maybe(..))
import Data.Either (either, Either(..))
import Data.Foldable (fold)
import Control.Monad.Trans.Class (lift)

data LogType
  = Debug
  | Info
  | Warn
  | Error

derive instance Eq LogType
derive instance Ord LogType

newtype LogEntry
  = LogEntry
  { logType :: LogType
  , logTimestamp :: DateTime
  , logContents :: String
  }

logType :: LogEntry -> LogType
logType (LogEntry { logType: e }) = e

logContents :: LogEntry -> String
logContents (LogEntry { logContents: c }) = c

formatLog :: LogType -> String
formatLog = case _ of
  Debug -> "DEBUG"
  Info -> "INFO"
  Warn -> "WARN"
  Error -> "ERROR"

mkLogEntry :: forall m. Now m => LogType -> String -> m LogEntry
mkLogEntry logType inputMessage = do
  now <- nowDateTime
  pure $ LogEntry { logType: logType, logTimestamp: now, logContents: headerWith now }
  where
  formatTimestamp =
    either (const "(Failed to assign time)") identity
      <<< formatDateTime "DD-MM-YYYY hh:mm:ss a"

  headerWith time =
    fold
      [ "["
      , formatLog logType
      , ": "
      , formatTimestamp time
      , "]\n"
      , inputMessage
      ]

class
  (Monad m, Now m) <= Log m where
  logMessage :: LogEntry -> m Unit

instance Log m => Log (HalogenM st act slots out m) where
  logMessage = lift <<< logMessage

log :: forall m. Log m => LogType -> String -> m Unit
log logType = logMessage <=< mkLogEntry logType

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
