{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Profile.ListCoaches where

import Pact.Web.Server.Handler.Prelude

getListCoachesR :: Handler Html
getListCoachesR = do
  token <- genToken
  user <- getUser
  coaches <- runDB $ selectListVals [] []
  relations <- runDB $ selectListVals [CustomerCoachRelationCustomer ==. userUuid user] []
  coachInfos <- fmap catMaybes $
    forM coaches $ \coach -> do
      mUser <- runDB . getBy . UniqueUser $ coachUser coach
      case mUser of
        Just (Entity _ coachUser) -> pure $ Just (coachUser, coach)
        Nothing -> pure Nothing
  defaultLayout $ do
    setTitleI ("Coaches" :: Text)
    $(widgetFile "profile/listCoaches")

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
