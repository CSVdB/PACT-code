module PACT.Component.HTML.Header where

import Prelude
import PACT.Data.Router (Route(..))
import PACT.Data.User (Profile)
import PACT.Component.HTML.Utils (css, safeHref, whenElem)
import Data.Maybe (Maybe, isJust, isNothing)
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

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
            [ HH.img
              [ HP.src logoSrc
              , HP.width 220
              , HP.height 150
             ] ]
            -- , HH.div
            --     [ css "text-xs-center" ]
            --     [ HH.text "Building an energized and inspired life, together!" ]
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
  logoSrc = "https://i.ibb.co/J3VQpX4/209095110-526812831853287-3496152201546800433-n.jpg"
