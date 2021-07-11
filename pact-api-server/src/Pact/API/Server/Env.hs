module Pact.API.Server.Env where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sql
import Pact.DB
import Pact.Data
import Servant
import Servant.Auth.Server

type H = LoggingT (ReaderT Env Handler)

data Env = Env
  { envConnectionPool :: ConnectionPool,
    envCookieSettings :: CookieSettings,
    envJWTSettings :: JWTSettings
  }

-- This automatically logs using `MonadLogger`.
runDB :: SqlPersistT IO a -> H a
runDB func = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool func pool

getUser :: Username -> H (Maybe (Entity User))
getUser un = runDB . getBy $ UniqueUsername un

getProfile :: Username -> H Profile
getProfile un = do
  mu <- getUser un
  case mu of
    Nothing -> throwError err404
    Just user -> pure . toProfile $ entityVal user

withUser :: Username -> (Profile -> H a) -> H a
withUser un func = func =<< getProfile un
