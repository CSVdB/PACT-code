{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Home where

import Pact.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  messages <- getMessages
  setTitle "Home"
  $(widgetFile "home")
