{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Home where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Time.Clock
import Pact.Web.Server.Handler.Prelude

getHomeR :: Handler Html
getHomeR = do
  getUserType >>= \case
    Nobody -> nonUserHomeR
    LoggedInUser user -> newsfeedR user Nothing
    LoggedInCoach user coach -> newsfeedR user $ Just coach

nonUserHomeR :: Handler Html
nonUserHomeR = defaultLayout $ do
  setTitle "Home"
  $(widgetFile "home")

newsfeedR :: User -> Maybe Coach -> Handler Html
newsfeedR _ mCoach = do
  customerCoachProposals <- case mCoach of
    Nothing -> pure []
    Just coach -> runDB $ collectCustomerCoachProposals coach
  token <- genToken
  today <- liftIO $ utctDay <$> getCurrentTime
  userWorkouts <-
    sortOn (Down . userWorkoutDay . snd)
      <$> (runDB . getLastWeeksWorkouts today =<< getUser)
  defaultLayout $ do
    setTitle "PACT"
    $(widgetFile "newsfeed")
