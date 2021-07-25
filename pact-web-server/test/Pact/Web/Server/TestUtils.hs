module Pact.Web.Server.TestUtils where

import Control.Monad.Logger
import Pact.Web.Server.Application ()
import Pact.Web.Server.Foundation
import Pact.Web.Server.Static
import Test.Hspec
import Yesod.Test

type PactWebServerSpec = YesodSpec App

type PactWebServerExample = YesodExample App

pactWebServerSpec :: PactWebServerSpec -> Spec
pactWebServerSpec =
  yesodSpec $
    App
      { appLogLevel = LevelWarn,
        appStatic = pactWebServerStatic,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }
