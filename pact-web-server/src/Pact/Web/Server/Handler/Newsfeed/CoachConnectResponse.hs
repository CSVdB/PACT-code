{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Newsfeed.CoachConnectResponse where

import Pact.Web.Server.Handler.Prelude

postConnectCoachResponseR :: UserUUID -> ProposalResponse -> Handler Html
postConnectCoachResponseR uuid response =
  getCoach >>= \(_, Coach {..}) ->
    runDB (respondToProposal uuid coachUuid response) >>= \case
      SqlSuccess -> redirect HomeR
      SqlNotFound -> do
        addMessage "is-danger" "This customer didn't ask you to be his coach"
        notFound
      SqlAlreadyUpdated -> do
        addMessage "is-danger" "You already responded to this customer"
        notFound
