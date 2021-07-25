{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Foundation where

import Data.Text (Text)
import Pact.Web.Server.Static
import Pact.Web.Server.Widget
import Text.Hamlet
import Yesod
import Yesod.EmbeddedStatic

data App = App
  { appLogLevel :: !LogLevel,
    appStatic :: !EmbeddedStatic,
    appGoogleAnalyticsTracking :: !(Maybe Text),
    appGoogleSearchConsoleVerification :: !(Maybe Text)
  }

mkYesodData "App" $(parseRoutesFile "routes.txt")

instance Yesod App where
  shouldLogIO app _ ll = pure $ ll >= appLogLevel app
  defaultLayout widget = do
    app <- getYesod
    currentRoute <- getCurrentRoute
    navbarPC <- widgetToPageContent $(widgetFile "navbar")
    pageContent <- widgetToPageContent $(widgetFile "default-body")
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")

getFaviconR :: Handler TypedContent
getFaviconR = redirect $ StaticR logo_jpg

navbarRoutesNotLoggedIn :: [(Route App, String)]
navbarRoutesNotLoggedIn =
  [ (HomeR, "Home"),
    (LoginR, "Log in"),
    (RegisterR, "Sign up")
  ]
