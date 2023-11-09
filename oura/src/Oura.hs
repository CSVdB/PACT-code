{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Oura where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Function
import Data.Proxy
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Database.Persist.Sqlite
import Lens.Micro ((.~))
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Pact.DB.Oura
import Pact.Data.Oura
import Servant.API
import Servant.Client
import System.Exit (die)

-- TODO:
-- - Run script for all OURA_TOKENS: Get connection pool
-- - Set dB path properly:
-- - Push data into dB
-- - E2E test dB integration
-- - Include yesterday's scores in your profile
-- - Set up service to run script every day
-- - Deploy & check if everything is working

type ReqParam = QueryParam' '[Required, Strict]

type OuraHeader = Header' '[Required, Strict] "Authorization" OuraToken

type ActivityEndpoint = "daily_activity" :> ReqParam "start_date" Day :> ReqParam "end_date" Day :> OuraHeader :> Get '[JSON] ActivityData

type SleepEndpoint = "daily_sleep" :> ReqParam "start_date" Day :> ReqParam "end_date" Day :> OuraHeader :> Get '[JSON] SleepData

type ReadinessEndpoint = "daily_readiness" :> ReqParam "start_date" Day :> ReqParam "end_date" Day :> OuraHeader :> Get '[JSON] ReadinessData

type OuraAPI = "v2" :> "usercollection" :> (ActivityEndpoint :<|> SleepEndpoint :<|> ReadinessEndpoint)

ouraAPI :: Proxy OuraAPI
ouraAPI = Proxy

clientCollectActivityData :: Day -> Day -> OuraToken -> ClientM ActivityData
clientCollectSleepData :: Day -> Day -> OuraToken -> ClientM SleepData
clientCollectReadinessData :: Day -> Day -> OuraToken -> ClientM ReadinessData
clientCollectActivityData :<|> clientCollectSleepData :<|> clientCollectReadinessData = client ouraAPI

runClientM' :: ClientEnv -> ClientM a -> IO a
runClientM' env c =
  runClientM c env >>= \case
    Left err -> die $ show err
    Right a -> pure a

ouraSync :: IO ()
ouraSync = do
  man <- newManager tlsManagerSettings
  url <- parseBaseUrl "https://api.ouraring.com"
  let clientEnv = mkClientEnv man url
  today <- utctDay <$> getCurrentTime
  let lastMonth = addGregorianMonthsRollOver (-1) today
  runStderrLoggingT $ filterLogger (\_ ll -> ll >= logLevel) $ withSqlitePoolInfo info 1 $ \pool -> do
    tokens <- liftIO $ runSqlPool collectOuraTokens pool
    forM_ tokens $ \(_userId, token) -> liftIO $ do
      let ouraToken = "Bearer " <> token
      activityData <- runClientM' clientEnv $ clientCollectActivityData lastMonth today ouraToken
      sleepData <- runClientM' clientEnv $ clientCollectSleepData lastMonth today ouraToken
      readinessData <- runClientM' clientEnv $ clientCollectReadinessData lastMonth today ouraToken
      let scores = combineScores activityData sleepData readinessData
      print scores
  where
    info =
      mkSqliteConnectionInfo (T.pack "./pact.sqlite3")
        & walEnabled .~ False
        & fkEnabled .~ False
    logLevel = LevelWarn
