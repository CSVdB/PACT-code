{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Profile.ListFriends where

import Pact.Web.Server.Handler.Prelude

getListFriendsR :: Handler Html
getListFriendsR = do
  token <- genToken
  user <- getUser
  users <- runDB $ selectListVals [] []
  relations <- runDB $ collectFriendInfos user
  let proposedFriends = friendFRI <$> relations
      notProposedUsers = users \\ proposedFriends
  defaultLayout $ do
    setTitleI ("Coaches" :: Text)
    $(widgetFile "profile/listFriends")
