{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Coach
  ( getProfileR,
    postProfileR,
    ProfileForm (..),
    getListR,
  )
where

import Data.Maybe (catMaybes)
import Pact.Web.Server.Handler.Coach.Profile
import Pact.Web.Server.Handler.Import

getListR :: Handler Html
getListR = do
  coaches <- fmap (fmap entityVal) $ runDB $ selectList [] []
  coachInfos <- fmap catMaybes $
    forM coaches $ \coach -> do
      mUser <- fmap (fmap entityVal) . runDB . getBy . UniqueUserUUID $ coachUser coach
      case mUser of
        Just user -> pure $ Just (user, coach)
        Nothing -> pure Nothing
  defaultLayout $ do
    setTitleI ("Coaches" :: Text)
    $(widgetFile "coach/list")
