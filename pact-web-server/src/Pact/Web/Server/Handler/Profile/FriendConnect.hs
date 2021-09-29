{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Profile.FriendConnect where

import Pact.Web.Server.Handler.Prelude

postConnectFriendR :: UserUUID -> Handler Html
postConnectFriendR uuid = do
  User {..} <- getUser
  runDB (getBy $ UniqueFriend userUuid uuid) >>= \case
    Just _ -> do
      addMessage "is-danger" "You already proposed to connect to this user"
      notFound
    Nothing -> pure ()
  runDB (getBy $ UniqueUser uuid) >>= \case
    Nothing -> notFound
    Just _ -> pure ()
  runDB . insert_ $
    FriendRelation
      { friendRelationProposer = userUuid,
        friendRelationReceiver = uuid,
        friendRelationResponse = Nothing
      }
  redirect $ ProfileR ListFriendsR
