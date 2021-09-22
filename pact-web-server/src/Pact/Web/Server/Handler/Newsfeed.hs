{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Newsfeed
  ( newsfeedR,
    postConnectCoachResponseR,
    getAddUserWorkoutR,
    postAddUserWorkoutR,
    AddUserWorkoutForm (..),
  )
where

import Data.Time.Clock
import Pact.Web.Server.Handler.Newsfeed.CoachConnectResponse
import Pact.Web.Server.Handler.Newsfeed.Workout
import Pact.Web.Server.Handler.Prelude

newsfeedR :: User -> Maybe Coach -> Handler Html
newsfeedR user mCoach = do
  customerCoachProposals <- fmap (fromMaybe []) $
    forM mCoach $ \coach ->
      runDB $ collectCustomerCoachProposals coach
  token <- genToken
  today <- liftIO $ utctDay <$> getCurrentTime
  userWorkouts <-
    sortOn (Down . userWorkoutDay . snd)
      <$> runDB (getLastWeeksWorkouts today user)
  defaultLayout $ do
    setTitle "PACT"
    $(widgetFile "newsfeed")
