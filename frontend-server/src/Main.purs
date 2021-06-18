module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Component (ComponentSlot)
import Halogen.Query.HalogenQ (HalogenQ)
import Halogen.VDom.Driver (runUI)

-- Global state of the app ("Env")
data Store
  = Store

type State
  = Unit

data Action
  = Initialize
  | Receive Unit

-- The world talks to the component
data Query a
  = NoOp a

-- A parent talks to the component
type Input
  = Unit

-- Talk to the parent component
type Output
  = Void

-- Opaque components are ones with no queries or messages.
type OpaqueSlot slot
  = forall query. H.Slot query Void slot

-- Used to distinguish between two child types that have -- the same query type
type ChildSlots
  = ( home :: OpaqueSlot Unit
    )

initialState :: Input -> State
initialState = const unit

eval :: HalogenQ Query Action Input ~> H.HalogenM State Action ChildSlots Output Aff
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
  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void Aff (Maybe a)
  handleQuery (NoOp a) = pure $ Just a

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void Aff Unit
  handleAction Initialize = pure unit

  handleAction (Receive _) = pure unit

  receive :: Input -> Maybe Action
  receive = const Nothing

  initialize :: Maybe Action
  initialize = Just Initialize

render :: State -> HH.HTML (ComponentSlot ChildSlots Aff Action) Action
render = const $ HH.div_ [ HH.text "Hello, world!" ]

rootComponent :: H.Component Query Input Output Aff
rootComponent = H.mkComponent { initialState, render, eval }

main :: Effect Unit
main = do
  log "And we're launching!"
  HA.runHalogenAff do
    -- Get the HTML element for Halogen to control
    htmlBody <- HA.awaitBody
    -- -- We now have to link a Halogen component with `htmlBody`, usually the
    -- -- router component.
    -- let
    --   initialStore = Store
    -- rootComponent <- runAppM initialStore Router.component
    _ <- runUI rootComponent unit htmlBody -- Bind component to htmlBody
    -- This result (`halogenIO`) now has two records:
    -- * `query`: Send queries to the root component
    -- * `subscribe`: Listen and react to root component's output
    pure unit

-- where
--   baseUrl = BaseURL "localhost"
--   logLevel = Dev
