module PACT.Component.Router where

import Prelude
import PACT.AppM (AppM)
import PACT.Data.Router (Route(..), routeCodec)
import PACT.Page.Register as Register
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (hush)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Component (ComponentSlot)
import Halogen.Query.HalogenQ (HalogenQ)
import Routing.Hash (setHash, getHash)
import Routing.Duplex as RD
import Type.Proxy (Proxy(..))

type State =
  { route :: Maybe Route
  }

data Action
  = Initialize

-- The world talks to the component
data Query a
  = Navigate Route a

-- A parent talks to the component
type Input
  = Route

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
    )

eval :: HalogenQ Query Action Input ~> H.HalogenM State Action ChildSlots Output AppM
eval =
  H.mkEval
    $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction =
          handleAction
        , receive = receive
        , initialize = initialize
        }
  where
  -- If the `a` in `Maybe a` is `Nothing`, the component doesn't re-render.
  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void AppM (Maybe a)
  handleQuery (Navigate dest a) = do
    { route: currRoute } <- H.get
    -- Don't re-render unless the route has changed
    when (currRoute /= Just dest)
      $ do H.modify_ $ _ { route = Just dest }
    pure $ Just a

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void AppM Unit
  handleAction Initialize = do
    -- Get route the user landed on
    initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
    -- Navigate to this route. If it doesn't exist, go to `Home`.
    H.liftEffect <<< setHash <<< RD.print routeCodec $ fromMaybe Home initialRoute

  receive :: Input -> Maybe Action
  receive = const Nothing

  initialize :: Maybe Action
  initialize = Just Initialize

render :: State -> HH.HTML (ComponentSlot ChildSlots AppM Action) Action
render { route: Nothing } = HH.div_ [ HH.text "Oh no! That page wasn't found." ]

render { route: Just r } = case r of
  Home -> HH.div_ [ HH.text "Hello, world!" ]
  Login -> HH.div_ [ HH.text "Welcome to the login page!" ]
  Register -> HH.slot_ (Proxy :: _ "register") unit Register.component unit

component :: H.Component Query Input Output AppM
component = H.mkComponent { initialState, render, eval }
  where
    initialState = const { route: Just Home }
