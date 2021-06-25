module PACT.Component.Router where

import Prelude
import PACT.Data.Router (Route(..), routeCodec)
import PACT.Data.User (Profile)
import PACT.Store as Store
import PACT.Capability.Log (class Log)
import PACT.Capability.Navigate (class Navigate)
import PACT.Capability.User (class ManageUser)
import PACT.Page.Home as Home
import PACT.Page.Login as Login
import PACT.Page.Register as Register
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Either (hush)
import Data.Foldable (elem)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Select (selectEq)
import Halogen.Store.Monad (class MonadStore)
import Routing.Hash (setHash, getHash)
import Routing.Duplex as RD
import Type.Proxy (Proxy(..))

type State
  = { route :: Maybe Route
    -- We want to expand `Input` (using `connect`) to make changes in
    -- `currentUser` trigger a change in the component.
    , currentUser :: Maybe Profile
    }

data Action
  = Initialize
  | Receive (Connected (Maybe Profile) Unit)

-- The world talks to the component
data Query a
  = Navigate Route a

-- A parent talks to the component
type Input
  = Unit

-- Talk to the parent component
type Output
  = Void

-- Opaque components are ones with no queries or messages.
type OpaqueSlot slot
  = forall query. H.Slot query Void slot

-- Used to distinguish between two child types that have the same query type.
type ChildSlots
  = ( home :: OpaqueSlot Unit
    , login :: OpaqueSlot Unit
    , register :: OpaqueSlot Unit
    , greet :: OpaqueSlot Unit
    )

-- The pages we route to
type Page m
  = H.ComponentHTML Action ChildSlots m

component ::
  forall m.
  MonadAff m =>
  MonadStore Store.StoreAction Store.Store m =>
  Log m =>
  Navigate m =>
  ManageUser m =>
  H.Component Query Input Output m
component =
  connect (selectEq _.currentUser)
    $ H.mkComponent
        { initialState: \{ context: currentUser } -> { route: Nothing, currentUser }
        , render
        , eval:
            H.mkEval
              $ H.defaultEval
                  { handleQuery = handleQuery
                  , handleAction = handleAction
                  , receive = Just <<< Receive
                  , initialize = Just Initialize
                  }
        }
  where
  -- If the `a` in `Maybe a` is `Nothing`, the component doesn't re-render.
  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get
      when (route /= Just dest)
        $ case (isJust currentUser && dest `elem` [ Login, Register ]) of
            false -> H.modify_ (_ { route = Just dest })
            -- Don't go to login or registration page if you're already logged in.
            _ -> pure unit
      pure $ Just a -- Don't re-render unless the route has changed

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- Get route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
      -- Navigate to this route. If it doesn't exist, go to `Home`.
      H.liftEffect <<< setHash <<< RD.print routeCodec $ fromMaybe Home initialRoute
    Receive { context: currentUser } -> H.modify_ _ { currentUser = currentUser }

  authorize :: Maybe Profile -> Page m -> Page m
  authorize mProfile html = case mProfile of
    Nothing -> HH.slot (Proxy :: _ "login") unit Login.component { redirect: true } absurd
    Just _ -> html

  render :: State -> Page m
  render { route: Nothing, currentUser: _ } = HH.div_ [ HH.text "Oh no! That page wasn't found." ]

  render { route: Just r, currentUser: mProfile } = case r of
    Home -> HH.slot_ (Proxy :: _ "home") unit Home.component unit
    Login -> HH.slot_ (Proxy :: _ "login") unit Login.component { redirect: true }
    Register -> HH.slot_ (Proxy :: _ "register") unit Register.component unit
    Greet -> authorize mProfile $ HH.div_ [ HH.text "Greetings!" ]
