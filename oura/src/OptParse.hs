-- Note: There is no config for this, on purpose.
module OptParse where

import Control.Monad
import qualified Env
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import System.Exit

newtype Flags = Flags
  { flagsDbPath :: Maybe FilePath
  }
  deriving (Show)

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "db-path",
                help "Path to the database file",
                metavar "FILEPATH"
              ]
          )
      )

flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr = unlines [Env.helpDoc envParser]

getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser
  where
    prefs_ =
      OptParse.defaultPrefs
        { OptParse.prefShowHelpOnError = True,
          OptParse.prefShowHelpOnEmpty = True
        }

newtype Environment = Environment
  { envDbPath :: Maybe FilePath
  }
  deriving (Show)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") envParser

envParser :: Env.Parser Env.Error Environment
envParser =
  Env.prefixed "OURA" $
    Environment
      <$> Env.var (fmap Just . Env.str) "DB_FILE" (Env.def Nothing <> Env.help "Database file")

newtype Settings = Settings
  { settingsDbPath :: Path Abs File
  }
  deriving (Show)

combineToSettings :: Flags -> Environment -> IO Settings
combineToSettings flags env = do
  case flagsDbPath flags <|> envDbPath env of
    Just dbPath -> Settings <$> parseAbsFile dbPath
    Nothing -> do
      dbPath <- resolveFile' "pact.sqlite3"
      exists <- doesFileExist dbPath
      when (not exists) $ die "No database path was passed along, and there is no pact.sqlite3 file in the working directory"
      pure $ Settings dbPath

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  combineToSettings flags env
