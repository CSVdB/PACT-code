{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Profile.FriendConnect where

import Pact.Web.Server.Handler.Prelude

postConnectFriendR :: UserUUID -> Handler Html
postConnectFriendR uuid = do
  User {..} <- getUser
  runDB (getBy $ UniqueFriend userUuid uuid) >>= \friend -> when (isJust friend) $ do
    addMessage "is-danger" "You already proposed to connect to this user"
    notFound
  runDB (getBy $ UniqueUser uuid) >>= \user -> when (isNothing user) notFound
  runDB . insert_ $
    FriendRelation
      { friendRelationProposer = userUuid,
        friendRelationReceiver = uuid,
        friendRelationResponse = Nothing
      }
  redirect $ ProfileR ListFriendsR
