{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Exercise.Add where

import Pact.Web.Server.Handler.Import

getAddR :: Handler Html
getAddR = defaultLayout $ do
  setTitle "Add exercise"
  $(widgetFile "exercise/add")
