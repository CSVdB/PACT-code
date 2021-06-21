module PACT.Data.Log
  ( LogType(..)
  , logType
  , logContents
  , LogEntry
  , mkLogEntry
  ) where

import Prelude
import PACT.Capability.Now (class Now, nowDateTime)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (formatDateTime)
import Data.Foldable (fold)
import Data.Either (either)

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
