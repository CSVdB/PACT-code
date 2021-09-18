{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Coach
  ( getListR,
    postConnectR,
    ProfileForm (..),
    postConnectResponseR,
    module Pact.Web.Server.Handler.Coach.Activities,
    module Pact.Web.Server.Handler.Coach.Profile,
  )
where

import Data.Maybe (catMaybes)
import Pact.Web.Server.Handler.Coach.Activities
import Pact.Web.Server.Handler.Coach.Profile
import Pact.Web.Server.Handler.Prelude

getRelationTextIfExists :: User -> Coach -> [CustomerCoachRelation] -> Maybe Text
getRelationTextIfExists User {..} Coach {..} relations =
  case filter myFilter relations of
    [relation] -> case customerCoachRelationResponse relation of
      Nothing -> Just "Proposed"
      Just AcceptProposal -> Just "Accepted"
      Just DenyProposal -> Just "Denied"
    _ -> Nothing
  where
    myFilter CustomerCoachRelation {..} = customerCoachRelationCoach == coachUuid && customerCoachRelationCustomer == userUuid

getListR :: Handler Html
getListR = do
  token <- genToken
  user <- getUser
  coaches <- fmap (fmap entityVal) $ runDB $ selectList [] []
  relations <-
    fmap (fmap entityVal) . runDB $
      selectList [CustomerCoachRelationCustomer ==. userUuid user] []
  coachInfos <- fmap catMaybes $
    forM coaches $ \coach -> do
      mUser <- fmap (fmap entityVal) . runDB . getBy . UniqueUserUUID $ coachUser coach
      case mUser of
        Just coachUser -> pure $ Just (coachUser, coach)
        Nothing -> pure Nothing
  defaultLayout $ do
    setTitleI ("Coaches" :: Text)
    $(widgetFile "coach/list")

postConnectR :: CoachUUID -> Handler Html
postConnectR uuid = do
  mAuth <- maybeAuth
  case mAuth of
    Nothing -> notFound -- This will never happen because of yesod authorization
    Just (Entity _ User {..}) ->
      runDB (getBy $ UniqueRelation userUuid uuid)
        >>= \case
          Just _ -> notFound -- Can't propose again to the same coach
          Nothing -> do
            runDB $
              insert_
                CustomerCoachRelation
                  { customerCoachRelationCustomer = userUuid,
                    customerCoachRelationCoach = uuid,
                    customerCoachRelationResponse = Nothing
                  }
            redirect $ CoachR ListR

postConnectResponseR :: UserUUID -> ProposalResponse -> Handler Html
postConnectResponseR uuid response =
  getUserType >>= \case
    LoggedInCoach _ Coach {..} ->
      runDB (respondToProposal uuid coachUuid response) >>= \case
        SqlSuccess -> redirect HomeR
        SqlNotFound -> notFound
        SqlAlreadyUpdated -> notFound
    _ -> notFound -- This will never happen because of yesod authorization
