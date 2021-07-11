{-# LANGUAGE RecordWildCards #-}

module Pact.API.Server.Handler.Number where

import Pact.API.Server.Handler.Import

handlePostNumber :: AuthCookie -> Int -> H NoContent
handlePostNumber AuthCookie {..} n =
  withUser authCookieUsername $ \_ -> do
    runDB . insert_ $ MyRandomInt n
    pure NoContent
