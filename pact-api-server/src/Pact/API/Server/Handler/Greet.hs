{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.API.Server.Handler.Greet where

import Pact.API.Server.Handler.Import

handleGetGreet :: AuthCookie -> H Text
handleGetGreet AuthCookie {..} =
  withUser authCookieUsername $ \_ ->
    pure $ "Hello " <> usernameText authCookieUsername
