{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Profile.BecomeCoach where

import Pact.Web.Server.Handler.Prelude

postBecomeCoachR :: Handler Html
postBecomeCoachR =
  getCoachM >>= \case
    (_, Just _) -> do
      addMessage "is-danger" "You're already a coach!"
      redirect $ ProfileR ProfilePageR
    (User {..}, Nothing) -> do
      uuid <- nextRandomUUID
      runDB $ insert_ Coach {coachUser = userUuid, coachUuid = uuid, coachExpertise = ""}
      addMessage "is-success" "Congratulations, you're a coach now!"
      redirect $ ProfileR ProfilePageR
