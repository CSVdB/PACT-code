{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Activities.ListParticipants where

import Pact.Web.Server.Handler.Prelude

getListParticipantsCoachWorkoutR :: CoachWorkoutUUID -> Handler Html
getListParticipantsCoachWorkoutR workoutUUID = do
  infoM <- runDB $ getCoachWorkoutInfoFromUUID workoutUUID
  case infoM of
    Nothing -> notFound
    Just info -> defaultLayout $ do
      token <- genToken
      setTitle "Participants"
      $(widgetFile "activities/list-participants")
