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

getProfilePageR :: Handler Html
getProfilePageR = do
  user <- getUser
  token <- genToken
  defaultLayout $ do
    setTitle "Profile"
    $(widgetFile "profile")
