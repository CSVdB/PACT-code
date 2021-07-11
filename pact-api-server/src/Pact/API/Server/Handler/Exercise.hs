{-# LANGUAGE RecordWildCards #-}

module Pact.API.Server.Handler.Exercise where

import Pact.API.Server.Handler.Import

handlePostExercise :: AuthCookie -> Exercise -> H NoContent
handlePostExercise AuthCookie {..} exercise =
  withUser authCookieUsername $ \_ -> do
    runDB $ insert_ exercise
    pure NoContent
