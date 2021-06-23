module PACT.AppM where

import Prelude
import PACT.Router as Route
import PACT.Data.User (Profile)
import PACT.Capability.Now (class Now)
import PACT.Capability.Log (class Log)
import PACT.Capability.Log as Log
import PACT.Capability.Navigate (class Navigate, navigate)
import PACT.API.Request (BaseURL, removeToken)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT, getStore, updateStore)
import Effect.Aff (Aff)
import Effect.Console as Console
import Safe.Coerce (coerce)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Now
import Routing.Duplex (print)
import Routing.Hash (setHash)

data LogLevel
  = Dev
  | Prod

derive instance Eq LogLevel
derive instance Ord LogLevel

-- An environment to the app, available to all components who wish it so.
type Store =
  { logLevel :: LogLevel
  , currentUser :: Maybe Profile
  , baseURL :: BaseURL
  }

-- An action that can update the store.
data StoreAction
  = LoginUser Profile
  | LogoutUser

reduce :: Store -> StoreAction -> Store
reduce store = case _ of
  LoginUser profile -> store { currentUser = Just profile }
  LogoutUser -> store { currentUser = Nothing }

-- Our app's core monad, in which the production app will run.
newtype AppM a
  = AppM (StoreT StoreAction Store Aff a)

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadStore StoreAction Store AppM

-- Note: `coerce` here turns `AppM` into `StoreT Store Aff`.
runAppM :: forall q i o. Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM initStore = runStoreT initStore reduce <<< coerce

instance Now AppM where
  nowDateTime = liftEffect Now.nowDateTime

instance Log AppM where
  logMessage entry = do
    { logLevel } <- getStore
    liftEffect $ case logLevel, Log.logType entry of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.logContents entry

instance Navigate AppM where
  navigate route = liftEffect $ setHash $ print Route.routeCodec route
  logoutUser = do
      liftEffect removeToken
      updateStore LogoutUser
      navigate Route.Home
