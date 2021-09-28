{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Profile
  ( getProfilePageR,
    postBecomeCoachR,
    postConnectCoachR,
    getListCoachesR,
    CoachProfileForm (..),
    UserProfileForm (..),
    getUpdateCoachProfileR,
    postUpdateCoachProfileR,
    getUpdateUserProfileR,
    postUpdateUserProfileR,
  )
where

import Pact.Web.Server.Handler.Prelude
import Pact.Web.Server.Handler.Profile.BecomeCoach
import Pact.Web.Server.Handler.Profile.CoachConnect
import Pact.Web.Server.Handler.Profile.CoachProfile
import Pact.Web.Server.Handler.Profile.ListCoaches
import Pact.Web.Server.Handler.Profile.UserProfile

getProfilePage :: Edit -> Handler Html
getProfilePage editProfile = do
  (user, mCoach) <- getCoachM
  case editProfile of
    -- You must be a coach to edit your coach profile
    CoachEdit -> when (isNothing mCoach) notFound
    _ -> pure ()

  customers <- case mCoach of
    Nothing -> pure []
    Just coach -> runDB $ collectCustomers coach
  coaches <- runDB $ collectCoachesAndUser user
  token <- genToken
  defaultLayout $ do
    setTitle "Profile"
    $(widgetFile "profile")

getProfilePageR :: Handler Html
getProfilePageR = getProfilePage NoEdit

getUpdateUserProfileR :: Handler Html
getUpdateUserProfileR = getProfilePage UserEdit

getUpdateCoachProfileR :: Handler Html
getUpdateCoachProfileR = getProfilePage CoachEdit
