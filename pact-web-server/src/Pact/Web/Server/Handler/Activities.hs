{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Activities
  ( getActivitiesPageR,
    postUpdateCoachWorkoutJoinR,
    postJoinCoachWorkoutR,
    getAddCoachWorkoutR,
    postAddCoachWorkoutR,
    AddCoachWorkoutForm (..),
  )
where

import Pact.Web.Server.Handler.Activities.Join
import Pact.Web.Server.Handler.Activities.UpdateJoin
import Pact.Web.Server.Handler.Activities.Workout
import Pact.Web.Server.Handler.Prelude

getActivitiesPageR :: Handler Html
getActivitiesPageR = do
  (user, mCoach) <- getCoachM
  nowLocal <- getLocalNow
  coachWorkoutInfos <-
    sortCWIs nowLocal . fromMaybe []
      <$> forM mCoach (runDB . getCoachWorkoutInfos)
  myPlannedWorkouts <-
    sortOn (timeCWI . snd) . filter (filterCondition nowLocal . snd)
      <$> runDB (userPlannedWorkouts user)
  myCoachesWorkoutInfos <- do
    infos <- sortCWIs nowLocal <$> runDB (getMyCoachesWorkoutInfos user)
    pure $ infos \\ (snd <$> myPlannedWorkouts)
  defaultLayout $ do
    token <- genToken
    setTitleI ("Activities" :: Text)
    $(widgetFile "activities")
  where
    sortCWIs nowLocal = sortOn timeCWI . filter (filterCondition nowLocal)
    filterCondition nowLocal CoachWorkoutInfo {..} = nowLocal <= timeCWI
