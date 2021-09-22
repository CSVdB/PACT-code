{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Activities.CancelUser where

import Pact.Web.Server.Handler.Prelude

postCancelCoachWorkoutJoinR :: CoachWorkoutUUID -> Handler Html
postCancelCoachWorkoutJoinR workoutUUID = do
  User {..} <- getUser
  runDB (getBy $ UniqueJoin userUuid workoutUUID) >>= \case
    Nothing -> notFound
    Just (Entity key WorkoutJoin {..}) -> case workoutJoinCancelled of
      Cancelled -> notFound
      NotCancelled -> do
        runDB $ update key [WorkoutJoinCancelled =. Cancelled]
        addMessage "is-success" "Oh, you cancelled :("
        redirect $ ActivitiesR ActivitiesPageR
