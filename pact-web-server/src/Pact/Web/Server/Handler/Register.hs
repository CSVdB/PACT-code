{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Register where

import Pact.Web.Server.Handler.Import

getRegisterR :: Handler Html
getRegisterR = defaultLayout $ do
  setTitle "Register"
  $(widgetFile "register")
