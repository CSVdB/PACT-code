{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Activities.Join where

import Pact.Web.Server.Handler.Prelude

postJoinCoachWorkoutR :: CoachWorkoutUUID -> Handler Html
postJoinCoachWorkoutR workoutUUID = do
  User {..} <- getUser
  runDB (getBy $ UniqueJoin userUuid workoutUUID) >>= \case
    Just _ -> notFound
    Nothing -> do
      runDB $
        insert_
          WorkoutJoin
            { workoutJoinCustomer = userUuid,
              workoutJoinWorkout = workoutUUID,
              workoutJoinStatus = WillCome
            }
      addMessage "is-success" "You joined a workout, this will be great fun!"
      redirect $ ActivitiesR ActivitiesPageR
