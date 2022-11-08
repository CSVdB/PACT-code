{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Profile.ListCurrentCoaches where

import Pact.Web.Server.Handler.Prelude

getListCurrentCoachesR :: Handler Html
getListCurrentCoachesR = do
  token <- genToken
  user <- getUser
  coaches <- runDB $ selectListVals [] []
  relations <- runDB $ selectListVals [CustomerCoachRelationCustomer ==. userUuid user] []
  -- The info on each coach who's currently a coach of this user.
  coachInfos <- fmap (filter (isCurrentCoach relations user . snd) . catMaybes) $
    forM coaches $
      \coach -> do
        mUser <- runDB . getBy . UniqueUser $ coachUser coach
        case mUser of
          Just (Entity _ coachUser) -> pure $ Just (coachUser, coach)
          Nothing -> pure Nothing
  defaultLayout $ do
    setTitleI ("Coaches" :: Text)
    $(widgetFile "profile/listCurrentCoaches")

isCurrentCoach :: [CustomerCoachRelation] -> User -> Coach -> Bool
isCurrentCoach relations User {..} Coach {..} =
  case filter myFilter relations of
    [relation] -> case customerCoachRelationResponse relation of
      Just AcceptProposal -> True -- This is a coach of this user
      _ -> False -- Coach request is not yet accepted or already rejected
    _ -> False -- No connection between the coach and user
  where
    myFilter CustomerCoachRelation {..} = customerCoachRelationCoach == coachUuid && customerCoachRelationCustomer == userUuid
