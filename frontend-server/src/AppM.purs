module PACT.AppM where

import Prelude
import PACT.Data.Router as Route
import PACT.Data.User (Profile)
import PACT.Store (LogLevel(..), Store, StoreAction(..), reduce)
import PACT.API.Request (writeToken, removeToken, login, register, Token)
import PACT.Capability.Now (class Now)
import PACT.Capability.Log (class Log)
import PACT.Capability.Log as Log
import PACT.Capability.User (class ManageUser)
import PACT.Capability.Navigate (class Navigate, navigate)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
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

processToken :: Either String (Tuple Token Profile) -> AppM (Maybe Profile)
processToken = case _ of
      Left err -> Log.logError err *> pure Nothing
      Right (Tuple token profile) -> do
         liftEffect $ writeToken token
         updateStore $ LoginUser profile
         pure $ Just profile

instance ManageUser AppM where
  loginUser form = do
    { baseURL } <- getStore
    login baseURL form >>= processToken

  registerUser form = do
    { baseURL } <- getStore
    register baseURL form >>= processToken

  getCurrentUser = map (_.currentUser) getStore
