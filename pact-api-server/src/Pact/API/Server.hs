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
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
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
    filterLogger (\_ ll -> ll >= settingsLogLevel) $
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
            Warp.run settingPort . loggingMiddleware . corsMiddleware $ pactAPIServerApp serverEnv

-- To make a custom logger, use `logStdoutDev { outputFormat =
-- customOutputFormat }`:
-- https://hackage.haskell.org/package/wai-extra-3.1.6/docs/Network-Wai-Middleware-RequestLogger.html#t:OutputFormat
loggingMiddleware :: Middleware
loggingMiddleware = logStdout

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-type"],
          corsMethods = ["GET", "POST", "HEAD", "DELETE", "PUT"],
          corsExposedHeaders = Just ["Content-type"]
        }

pactAPIServerApp :: Env -> Wai.Application
pactAPIServerApp env =
  genericServeTWithContext
    (flip runReaderT env . runStderrLoggingT)
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
