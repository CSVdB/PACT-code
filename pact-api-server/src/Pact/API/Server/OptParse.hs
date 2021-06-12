{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Pact.API.Server.OptParse where

import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import YamlParse.Applicative as YamlParse

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

data Settings = Settings
  { settingPort :: Int,
    settingDbFile :: Path Abs File,
    settingSigningKeyFile :: Path Abs File
  }
  deriving (Show, Eq, Generic)

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let settingPort = fromMaybe 8000 $ flagPort <|> envPort <|> mc configPort
  settingDbFile <-
    case flagDbFile <|> envDbFile <|> mc configDbFile of
      Nothing -> resolveFile' "pact.sqlite3"
      Just dbf -> resolveFile' dbf
  settingSigningKeyFile <- resolveFile' "signing-key.dat"
  pure Settings {..}
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

data Configuration = Configuration
  { configPort :: Maybe Int,
    configDbFile :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalField "port" "The port to serve api requests on"
        <*> optionalField "database" "Path to the database"

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= YamlParse.readConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      YamlParse.readConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|optparse-pact|])
  resolveFile xdgConfigDir "config.yaml"

data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envPort :: Maybe Int,
    envDbFile :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "PACT_API_SERVER_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var
        (fmap Just . Env.auto)
        "PORT"
        (mE <> Env.help "The port to serve api requests on")
      <*> Env.var
        (fmap Just . Env.str)
        "DATABASE"
        (mE <> Env.help "Path to the database file")
  where
    mE = Env.def Nothing

getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    ( OptParse.fullDesc
        <> OptParse.footerDoc (Just $ OptParse.string footerStr)
    )
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (YamlParse.prettyColourisedSchemaDoc @Configuration)
        ]

data Flags = Flags
  { flagConfigFile :: Maybe FilePath,
    flagPort :: Maybe Int,
    flagDbFile :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          auto
          (mconcat [long "port", help "The port to serve api requests on"])
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "database",
                help "Path to the database",
                metavar "FILEPATH"
              ]
          )
      )
