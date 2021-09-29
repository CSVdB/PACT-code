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
    getListFriendsR,
    CoachProfileForm (..),
    UserProfileForm (..),
    getUpdateCoachProfileR,
    postUpdateCoachProfileR,
    getUpdateUserProfileR,
    postUpdateUserProfileR,
    postConnectFriendR,
  )
where

import Pact.Web.Server.Handler.Prelude
import Pact.Web.Server.Handler.Profile.BecomeCoach
import Pact.Web.Server.Handler.Profile.CoachConnect
import Pact.Web.Server.Handler.Profile.CoachProfile
import Pact.Web.Server.Handler.Profile.FriendConnect
import Pact.Web.Server.Handler.Profile.ListCoaches
import Pact.Web.Server.Handler.Profile.ListFriends
import Pact.Web.Server.Handler.Profile.UserProfile

getProfilePage :: Edit -> Handler Html
getProfilePage editProfile = do
  (user, mCoach) <- getCoachM
  -- You must be a coach to edit your coach profile
  when (editProfile == CoachEdit && isNothing mCoach) notFound

  customers <- fmap (fromMaybe []) $ forM mCoach $ runDB . collectCustomers
  coaches <- runDB $ collectCoachesAndUser user
  friendInfos <- runDB $ collectFriendInfos user
  let friends = friendFRI <$> filter isFriend friendInfos
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
