{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Login where

import Pact.Web.Server.Handler.Import

getLoginR :: Handler Html
getLoginR = defaultLayout $ do
  setTitle "Login"
  $(widgetFile "login")
