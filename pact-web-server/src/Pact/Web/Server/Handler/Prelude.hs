{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Prelude
  ( module X,
    Generic,
    getUser,
    getCoachM,
    getCoach,
    nextRandomUUID,
    hasDuplicates,
  )
where

import Control.Monad as X
import Data.Containers.ListUtils (nubOrd)
import Data.Functor as X ((<&>))
import Data.List as X (sortOn, (\\))
import Data.Maybe as X
import Data.Ord as X (Down (..))
import Data.Text as X (Text)
import Data.UUID.Typed (nextRandomUUID)
import Data.Validity as X
import Data.Validity.Time as X ()
import GHC.Generics (Generic)
import Pact.DB as X
import Pact.Data as X
import Pact.Web.Server.Foundation as X
import Pact.Web.Server.Static as X
import Pact.Web.Server.Widget as X
import Pact.Web.Server.Yesod as X
import Yesod as X hiding (check)
import Yesod.Auth as X

getUser :: Handler User
getUser = do
  mAuth <- maybeAuth
  case mAuth of
    Nothing -> notFound -- This should have been stopped by Yesod authorization anyway.
    Just (Entity _ user) -> pure user

getCoachM :: Handler (User, Maybe Coach)
getCoachM =
  getUserType >>= \case
    LoggedInCoach user coach -> pure (user, Just coach)
    LoggedInUser user -> pure (user, Nothing)
    _ -> notFound

getCoach :: Handler (User, Coach)
getCoach =
  getUserType >>= \case
    LoggedInCoach user coach -> pure (user, coach)
    _ -> notFound -- This should have been stopped by Yesod authorization anyway.

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates xs = xs /= nubOrd xs
