module Main where

import Prelude (Unit, unit, bind, discard, void, when, ($), (/=))
import PACT.AppM (runAppM)
import PACT.Store (LogLevel(..))
import PACT.Data.Router
import PACT.Component.Router as Router
import PACT.API.Request (currentUser, BaseURL(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (launchAff_)
import Routing.Hash (matchesWith)
import Routing.Duplex as RD

main :: Effect Unit
main = do
  log "And we're launching!"
  HA.runHalogenAff do
    -- Get the HTML element for Halogen to control
    htmlBody <- HA.awaitBody
    -- We now have to link a Halogen component with `htmlBody`, usually the
    -- router component.
    let
      baseUrl = BaseURL "localhost"
    user <- currentUser baseUrl
    let
      initialStore = { logLevel: Dev, currentUser: user, baseURL: baseUrl }
    rootComponent <- runAppM initialStore Router.component
    -- Bind component to htmlBody
    halogenIO <- runUI rootComponent unit htmlBody
    -- This result now has two records:
    -- * `query`: Send queries to the root component
    -- * `subscribe`: Listen and react to root component's output
    void $ H.liftEffect
      $ matchesWith (RD.parse routeCodec) \old new ->
          when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
