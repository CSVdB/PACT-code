{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Profile.CoachConnect where

import Pact.Web.Server.Handler.Prelude

postConnectCoachR :: CoachUUID -> Handler Html
postConnectCoachR uuid = do
  User {..} <- getUser
  runDB (getBy $ UniqueRelation userUuid uuid) >>= \relation -> when (isJust relation) $ do
    addMessage "is-danger" "You already proposed to connect to this coach"
    notFound
  runDB (getBy $ UniqueCoach uuid) >>= \coach -> when (isNothing coach) notFound
  runDB . insert_ $
    CustomerCoachRelation
      { customerCoachRelationCustomer = userUuid,
        customerCoachRelationCoach = uuid,
        customerCoachRelationResponse = Nothing
      }
  redirect $ ProfileR ListCoachesR
