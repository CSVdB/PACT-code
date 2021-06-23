{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.API.Server where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist.Sql
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Pact.API as API
import Pact.API.Server.Env
import Pact.API.Server.Handler
import Pact.API.Server.OptParse
import Pact.API.Server.SigningKey
import Path
import Servant.Auth.Server
import Servant.Server.Generic

pactAPIServer :: IO ()
pactAPIServer = do
  Settings {..} <- getSettings
  runStderrLoggingT $
    withSqlitePool (T.pack (fromAbsFile settingDbFile)) 1 $
      \pool -> do
        runSqlPool (runMigration serverMigration) pool
        liftIO $ do
          jwk <- loadSigningKey settingSigningKeyFile
          let serverEnv =
                Env
                  { envConnectionPool = pool,
                    envCookieSettings = defaultCookieSettings,
                    envJWTSettings = defaultJWTSettings jwk
                  }
          Warp.run settingPort $ pactAPIServerApp serverEnv

pactAPIServerApp :: Env -> Wai.Application
pactAPIServerApp env =
  genericServeTWithContext
    (flip runReaderT env)
    pactHandlers
    (pactContext env)

pactContext :: Env -> Context '[CookieSettings, JWTSettings]
pactContext Env {..} = envCookieSettings :. envJWTSettings :. EmptyContext

pactHandlers :: PactRoutes (AsServerT H)
pactHandlers =
  PactRoutes
    { postRegister = handlePostRegister,
      postLogin = handlePostLogin,
      getGreet = protected handleGetGreet,
      postNumber = protected handlePostNumber,
      getUser = protected handleGetUser
    }

protected :: ThrowAll m => (authCookie -> m) -> AuthResult authCookie -> m
protected func (Authenticated authCookie) = func authCookie
protected _ _ = throwAll err401
