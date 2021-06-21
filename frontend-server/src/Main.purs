module Main where

import Prelude
import AppM (runAppM, LogLevel(..), AppM)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (launchAff_)
import Router (Route(..), routeCodec)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Component (ComponentSlot)
import Halogen.Query.HalogenQ (HalogenQ)
import Halogen.VDom.Driver (runUI)
import Routing.Hash (getHash, setHash, matchesWith)
import Routing.Duplex as RD
import Data.Either (hush)

-- Global state of the app ("Env")
data Store
  = Store

type State
  = { route :: Maybe Route }

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

initialState :: Input -> State
initialState = const { route: Just Home }

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
  -- TODO: Figure out what happens to the `a` in `Maybe a`.
  -- If it's `Nothing`, the component doesn't re-render.
  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void AppM (Maybe a)
  handleQuery (Navigate dest a) = do
    { route: currRoute } <- H.get
    -- don't re-render unless the route has changed
    when (currRoute /= Just dest)
      $ do
          -- TODO: Same route changes aren't allowed and hence will be ignored.
          H.modify_ $ _ { route = Just dest }
    pure $ Just a

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void AppM Unit
  handleAction Initialize = do
    -- Get route the user landed on
    initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
    -- Navigate to this route
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
  Register -> HH.div_ [ HH.text "Welcome to the register page!" ]

rootComponent :: H.Component Query Input Output AppM
rootComponent = H.mkComponent { initialState, render, eval }

main :: Effect Unit
main = do
  log "And we're launching!"
  HA.runHalogenAff do
    -- Get the HTML element for Halogen to control
    htmlBody <- HA.awaitBody
    -- We now have to link a Halogen component with `htmlBody`, usually the
    -- router component.
    productionRootComponent <- runAppM initialStore rootComponent
    -- Bind component to htmlBody
    halogenIO <- runUI productionRootComponent Home htmlBody
    -- This result now has two records:
    -- * `query`: Send queries to the root component
    -- * `subscribe`: Listen and react to root component's output
    void $ H.liftEffect
      $ matchesWith (RD.parse routeCodec) \old new ->
          when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.mkTell $ Navigate new
  where
    initialStore = { logLevel: Dev }
