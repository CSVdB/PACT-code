{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Prelude
  ( module X,
    Generic,
    getUser,
    getCoachM,
    getCoach,
    workoutTypes,
  )
where

import Control.Monad as X
import Data.Text as X (Text)
import Data.Validity as X
import GHC.Generics (Generic)
import Pact.DB as X
import Pact.Data as X
import Pact.Web.Server.Foundation as X
import Pact.Web.Server.Static as X
import Pact.Web.Server.Widget as X
import Pact.Web.Server.Yesod as X
import Yesod as X hiding (check)
import Yesod.Auth as X

instance X.Validity Textarea where
  validate (Textarea t) = delve "Textarea" t

getUser :: Handler User
getUser = do
  mAuth <- maybeAuth
  case mAuth of
    Nothing -> notFound -- This should have been stopped by Yesod authorization anyway.
    Just (Entity _ user) -> pure user

getCoachM :: Handler (Maybe Coach)
getCoachM =
  getUserType >>= \case
    LoggedInCoach _ coach -> pure $ Just coach
    _ -> pure Nothing

getCoach :: Handler Coach
getCoach =
  getCoachM >>= \case
    Nothing -> notFound -- This should have been stopped by Yesod authorization anyway.
    Just coach -> pure coach

workoutTypes :: [WorkoutType]
workoutTypes = [minBound .. maxBound]
