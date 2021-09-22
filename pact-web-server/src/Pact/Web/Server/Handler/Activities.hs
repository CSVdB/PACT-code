{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Activities
  ( getActivitiesPageR,
    postCancelCoachWorkoutJoinR,
    postJoinCoachWorkoutR,
    getAddCoachWorkoutR,
    postAddCoachWorkoutR,
    AddCoachWorkoutForm (..),
  )
where

import Data.Time.Clock
import Pact.Web.Server.Handler.Activities.CancelUser
import Pact.Web.Server.Handler.Activities.Join
import Pact.Web.Server.Handler.Activities.Workout
import Pact.Web.Server.Handler.Prelude

getActivitiesPageR :: Handler Html
getActivitiesPageR = do
  (user, mCoach) <- getCoachM
  today <- liftIO $ utctDay <$> getCurrentTime
  coachWorkoutInfos <-
    sortCWIs today . fromMaybe []
      <$> forM mCoach (runDB . getCoachWorkoutInfos)
  myPlannedWorkouts <-
    sortOn (dayCWI . snd) . filter (filterCondition today . snd)
      <$> runDB (userPlannedWorkouts user)
  myCoachesWorkoutInfos <- do
    infos <- sortCWIs today <$> runDB (getMyCoachesWorkoutInfos user)
    pure $ infos \\ (snd <$> myPlannedWorkouts)
  defaultLayout $ do
    token <- genToken
    setTitleI ("Activities" :: Text)
    $(widgetFile "activities")
  where
    sortCWIs today = sortOn dayCWI . filter (filterCondition today)
    filterCondition today CoachWorkoutInfo {..} = today <= dayCWI
