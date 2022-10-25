{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Activities.ListParticipants where

import Pact.Web.Server.Handler.Prelude

getListParticipantsCoachWorkoutR :: CoachWorkoutUUID -> Handler Html
getListParticipantsCoachWorkoutR _workoutUUID = do
  pure ""
-- User {..} <- getUser
-- runDB (getBy $ UniqueJoin userUuid workoutUUID) >>= \case
--   Just _ -> notFound
--   Nothing -> do
--     runDB $ do
--       insert_
--         WorkoutJoin
--           { workoutJoinCustomer = userUuid,
--             workoutJoinWorkout = workoutUUID,
--             workoutJoinStatus = WillCome
--           }
--     addMessage "is-success" "You joined a workout, this will be great fun!"
--     redirect $ ActivitiesR ActivitiesPageR
