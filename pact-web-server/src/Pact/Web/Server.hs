{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server where

import Control.Monad
import Control.Monad.Logger
import Data.Function ((&))
import qualified Data.Text as T
import Database.Persist.Sqlite
import Lens.Micro ((.~))
import Network.HTTP.Client.TLS as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Pact.DB
import Pact.DB.Migrations
import Pact.Web.Server.Application ()
import Pact.Web.Server.Constants
import Pact.Web.Server.Foundation
import Pact.Web.Server.OptParse
import Pact.Web.Server.Static
import Path
import Path.IO
import Text.Show.Pretty
import Yesod

pactWebServer :: IO ()
pactWebServer = do
  sets <- getSettings
  when development $ pPrint sets
  runPactWebServer sets

runPactWebServer :: Settings -> IO ()
runPactWebServer Settings {..} = runStderrLoggingT $
  filterLogger (\_ ll -> ll >= settingLogLevel) $
    withSqlitePoolInfo info 1 $ \pool -> do
      liftIO . flip runSqlPool pool $ do
        runMigration serverMigration
        appSpecificMigrations
      sessionKeyFile <- resolveFile settingArtifactsDir "client_session_key.aes"
      man <- HTTP.newTlsManager
      let app =
            App
              { appLogLevel = settingLogLevel,
                appStatic = pactWebServerStatic,
                appConnectionPool = pool,
                appHTTPManager = man,
                appSessionKeyFile = sessionKeyFile,
                appHashDifficulty = 10,
                appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
                appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
              }
      liftIO $ Yesod.toWaiAppPlain app >>= Warp.run settingPort . middles
  where
    info =
      mkSqliteConnectionInfo (T.pack (fromAbsDir settingArtifactsDir <> "pact.sqlite3"))
        & walEnabled .~ False
        & fkEnabled .~ False
    loggerMiddle = if development then logStdoutDev else logStdout
    middles = loggerMiddle . defaultMiddlewaresNoLogging
