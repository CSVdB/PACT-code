{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Web.Server.Handler.Newsfeed.CoachConnectResponse where

import Pact.Web.Server.Handler.Prelude

postConnectCoachResponseR :: UserUUID -> CoachCoachProposalResponse -> Handler Html
postConnectCoachResponseR uuid response =
  getCoach >>= \(_, coach) ->
    runDB (respondToProposal uuid coach response) >>= \case
      SqlSuccess -> redirect HomeR
      SqlNotFound -> do
        addMessage "is-danger" "This customer didn't ask you to be his coach"
        notFound
      SqlAlreadyUpdated -> do
        addMessage "is-danger" "You already responded to this customer"
        notFound
