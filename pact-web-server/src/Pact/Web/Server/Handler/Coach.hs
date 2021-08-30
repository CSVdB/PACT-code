{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Coach
  ( getProfileR,
    postProfileR,
    getListR,
    postConnectR,
    ProfileForm (..),
  )
where

import Data.Maybe (catMaybes)
import Pact.Web.Server.Handler.Coach.Profile
import Pact.Web.Server.Handler.Import

getListR :: Handler Html
getListR = do
  token <- genToken
  user <- getUser
  coaches <- fmap (fmap entityVal) $ runDB $ selectList [] []
  proposals <- runDB $ selectList [CustomerCoachProposalCustomer ==. userUuid user] []
  let proposedCoaches = customerCoachProposalCoach . entityVal <$> proposals
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
    Just (Entity _ User {..}) -> do
      runDB $
        insert_
          CustomerCoachProposal
            { customerCoachProposalCustomer = userUuid,
              customerCoachProposalCoach = uuid
            }
      redirect $ CoachR ListR
