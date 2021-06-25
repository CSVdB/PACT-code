module PACT.Component.HTML.Header where

import Prelude
import PACT.Data.Router (Route(..))
import PACT.Data.User (Profile)
import PACT.Component.HTML.Utils (css, safeHref, whenElem)
import Data.Maybe (Maybe, isJust, isNothing)
import Data.Monoid (guard)
import Halogen.HTML as HH

header :: forall i p. Maybe Profile -> Route -> HH.HTML i p
header currentUser route =
  HH.nav
    [ css "navbar navbar-light" ]
    [ HH.div
        [ css "container" ]
        [ HH.a
            [ css "navbar-brand"
            , safeHref Home
            ]
            [ HH.text "PACT" ]
        , HH.ul
            [ css "nav navbar-nav pull-xs-right" ]
            [ navItem Home [ HH.text "Home" ]
            , whenElem (isJust currentUser)
                $ navItem Greet
                    [ HH.i [ css "ion-compose" ] [ HH.text " Greet" ] ]
            , whenElem (isNothing currentUser)
                $ navItem Login
                    [ HH.text "Log in" ]
            , whenElem (isNothing currentUser)
                $ navItem Register
                    [ HH.text "Sign up" ]
            ]
        ]
    ]
  where
  navItem r html =
    HH.li
      [ css "nav-item" ]
      [ HH.a
          [ css $ "nav-link" <> guard (route == r) " active"
          , safeHref r
          ]
          html
      ]
