module Pact.API.Server.Env where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist.Sql
import Pact.API.Data
import Pact.API.Server.DB
import Servant
import Servant.Auth.Server

type H = ReaderT Env Handler

data Env = Env
  { envConnectionPool :: ConnectionPool,
    envCookieSettings :: CookieSettings,
    envJWTSettings :: JWTSettings
  }

runDB :: SqlPersistT IO a -> H a
runDB func = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool func pool

getUser :: Username -> H (Maybe (Entity User))
getUser name = runDB . getBy $ UniqueUsername name

getProfile :: Username -> H Profile
getProfile name = do
  mu <- getUser name
  case mu of
    Nothing -> throwError err404
    Just user -> pure . toProfile $ entityVal user

withUser :: Username -> (Profile -> H a) -> H a
withUser un func = func =<< getProfile un
