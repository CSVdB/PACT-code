{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Profile.ListCurrentFriends where

import Pact.Web.Server.Handler.Prelude

getListCurrentFriendsR :: Handler Html
getListCurrentFriendsR = do
  token <- genToken
  user <- getUser
  relations <- runDB $ collectFriendInfos user
  let friends = filter isCurrentFriend relations
  defaultLayout $ do
    setTitleI ("Coaches" :: Text)
    $(widgetFile "profile/listCurrentFriends")

isCurrentFriend :: FriendRequestInfo -> Bool
isCurrentFriend = (==) (Just AcceptFriend) . responseFRI
