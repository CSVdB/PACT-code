{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Web.Server.Handler.Newsfeed.FriendConnectResponse where

import Pact.Web.Server.Handler.Prelude

postConnectFriendResponseR :: UserUUID -> FriendRequestResponse -> Handler Html
postConnectFriendResponseR uuid response = do
  user <- getUser
  runDB (getBy $ UniqueUser uuid) >>= \mUser -> when (isNothing mUser) notFound
  runDB (respondToFriendRequest uuid user response) >>= \case
    SqlSuccess -> redirect HomeR
    SqlNotFound -> do
      addMessage "is-danger" "This customer didn't ask you to be his friend"
      notFound
    SqlAlreadyUpdated -> do
      addMessage "is-danger" "You already responded to this user"
      notFound
