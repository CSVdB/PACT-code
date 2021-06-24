module PACT.Store where

import Prelude
import PACT.Data.User (Profile)
import PACT.API.Request (BaseURL, removeToken)
import Data.Maybe (Maybe(..))
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT, getStore, updateStore)

data LogLevel
  = Dev
  | Prod

derive instance Eq LogLevel
derive instance Ord LogLevel

-- An environment to the app, available to all components who wish it so.
type Store =
  { logLevel :: LogLevel
  , baseURL :: BaseURL
  , currentUser :: Maybe Profile
  }

-- An action that can update the store.
data StoreAction
  = LoginUser Profile
  | LogoutUser

reduce :: Store -> StoreAction -> Store
reduce store = case _ of
  LoginUser profile -> store { currentUser = Just profile }
  LogoutUser -> store { currentUser = Nothing }
