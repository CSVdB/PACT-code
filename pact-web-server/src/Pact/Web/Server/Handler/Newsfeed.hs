{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Newsfeed
  ( newsfeedR,
    postConnectCoachResponseR,
    postConnectFriendResponseR,
    getAddUserWorkoutR,
    postAddUserWorkoutR,
    AddUserWorkoutForm (..),
  )
where

import Pact.Web.Server.Handler.Newsfeed.CoachConnectResponse
import Pact.Web.Server.Handler.Newsfeed.FriendConnectResponse
import Pact.Web.Server.Handler.Newsfeed.Workout
import Pact.Web.Server.Handler.Prelude

newsfeedR :: User -> Maybe Coach -> Handler Html
newsfeedR user mCoach = do
  customerCoachProposals <- fmap (fromMaybe []) $ forM mCoach $ runDB . collectCustomerCoachProposals
  proposedFriends <- runDB $ fmap friendFRI . filter isReceiver <$> collectFriendInfos user
  nowLocal <- getLocalNow
  allPlannedWorkouts <- runDB (userPlannedWorkouts user)
  let workoutsToConfirm = filter (toConfirm nowLocal) allPlannedWorkouts
  token <- genToken
  today <- getCurrentDay
  workouts <- sortOn (Down . dayW) <$> runDB (getLastWeeksWorkouts today user)
  defaultLayout $ do
    setTitle "PACT"
    $(widgetFile "newsfeed")
  where
    toConfirm nowLocal (status, info) = status == WillCome && nowLocal > timeCWI info
