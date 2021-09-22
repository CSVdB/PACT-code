{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Home where

import Pact.Web.Server.Handler.Newsfeed
import Pact.Web.Server.Handler.Prelude

getHomeR :: Handler Html
getHomeR =
  getUserType >>= \case
    Nobody -> nonUserHomeR
    LoggedInUser user -> newsfeedR user Nothing
    LoggedInCoach user coach -> newsfeedR user $ Just coach

nonUserHomeR :: Handler Html
nonUserHomeR = defaultLayout $ do
  setTitle "Home"
  $(widgetFile "home")
