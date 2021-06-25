module PACT.Page.Home where

import Prelude
import PACT.Store as Store
import PACT.Data.Router (Route(..))
import PACT.Data.User (Profile)
import PACT.Capability.Log (class Log)
import PACT.Component.HTML.Header (header)
import PACT.Component.HTML.Utils (css)
import Data.Maybe (Maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (connect)
import Halogen.Store.Select (selectEq)
import Halogen.Store.Monad (class MonadStore)

data Action
  = Unit

type State
  -- We want to expand `Input` (using `connect`) to make changes in
  -- `currentUser` trigger a change in the component.
  = { currentUser :: Maybe Profile }

type Input
  = Unit

-- The component for the full login page. Focus on rendering and navigation. The
-- functionality happens in the child component for the login form.
component ::
  forall q o m.
  MonadStore Store.StoreAction Store.Store m =>
  Log m => H.Component q Input o m
component =
  connect (selectEq _.currentUser)
    $ H.mkComponent
        { initialState: \{ context: currentUser } -> { currentUser }
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
  where
  container currentUser html =
    HH.div
      [ css "auth-page" ]
      [ header currentUser Home
      , HH.div
          [ css "container page" ]
          [ HH.div
              [ css "row" ]
              [ HH.div
                  [ css "col-md-6 offset-md-3 col-xs12" ]
                  html
              ]
          , HH.p_ [ HH.text "Let's make a PACT to change the world!" ]
          ]
      ]

  -- Ignores the state, because there is none.
  render { currentUser } =
    container currentUser
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Welcome to the PACT community!" ]
      ]

  handleAction = const $ pure unit
