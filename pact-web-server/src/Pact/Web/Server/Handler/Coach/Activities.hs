{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Coach.Activities
  ( getAddActivityR,
    postAddActivityR,
  )
where

import Pact.Web.Server.Handler.Prelude

getAddActivityR :: WorkoutType -> Handler Html
getAddActivityR workoutType = defaultLayout $ do
  messages <- getMessages
  token <- genToken
  setTitleI ("Activity" :: Text)
  $(widgetFile "coach/activities")

postAddActivityR :: WorkoutType -> Handler Html
postAddActivityR = undefined
