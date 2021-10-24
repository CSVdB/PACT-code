{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Activities.UpdateJoin where

import Pact.Web.Server.Handler.Prelude

postUpdateCoachWorkoutJoinR :: CoachWorkoutUUID -> JoinStatus -> Handler Html
postUpdateCoachWorkoutJoinR _ WillCome = notFound
postUpdateCoachWorkoutJoinR workoutUUID status = do
  User {..} <- getUser
  runDB (getBy $ UniqueJoin userUuid workoutUUID) >>= \case
    Nothing -> notFound
    Just (Entity key WorkoutJoin {..}) -> case workoutJoinStatus of
      WillCome -> do
        runDB $ update key [WorkoutJoinStatus =. status]
        case status of
          Cancelled -> do
            addMessage "is-success" "Oh, you cancelled :("
            redirect $ ActivitiesR ActivitiesPageR
          WasPresent -> do
            addMessage "is-success" "Awesome you could make it!"
            redirect HomeR
          WasAbsent -> do
            addMessage "is-success" "Too bad you couldn't make it!"
            redirect HomeR
          WillCome -> notFound
      -- WillCome isn't an option here. That's enforced in the first line of
      -- this function.
      _ -> notFound
