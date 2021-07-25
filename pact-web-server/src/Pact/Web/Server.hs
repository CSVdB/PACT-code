{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server where

import Control.Monad
import Pact.Web.Server.Application ()
import Pact.Web.Server.Constants
import Pact.Web.Server.Foundation
import Pact.Web.Server.OptParse
import Pact.Web.Server.Static
import Text.Show.Pretty
import Yesod

pactWebServer :: IO ()
pactWebServer = do
  sets <- getSettings
  when development $ pPrint sets
  runPactWebServer sets

runPactWebServer :: Settings -> IO ()
runPactWebServer Settings {..} = do
  let app =
        App
          { appLogLevel = settingLogLevel,
            appStatic = pactWebServerStatic,
            appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
            appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
          }
  Yesod.warp settingPort app
