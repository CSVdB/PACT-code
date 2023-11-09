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
import OptParse
import Pact.DB.Oura
import Pact.Data
import Path
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

collectScores :: Day -> Day -> UserUUID -> OuraToken -> ClientM OverallScores
collectScores start end userId token = do
  let ouraToken = "Bearer " <> token
  activityData <- clientCollectActivityData start end ouraToken
  sleepData <- clientCollectSleepData start end ouraToken
  readinessData <- clientCollectReadinessData start end ouraToken
  pure $ combineScores userId activityData sleepData readinessData

ouraSync :: IO ()
ouraSync = do
  sets <- getSettings
  print sets
  let info =
        mkSqliteConnectionInfo (T.pack . toFilePath $ settingsDbPath sets)
          & walEnabled .~ False
          & fkEnabled .~ False
  man <- newManager tlsManagerSettings
  url <- parseBaseUrl "https://api.ouraring.com"
  let clientEnv = mkClientEnv man url
  today <- utctDay <$> getCurrentTime
  let lastMonth = addGregorianMonthsRollOver (-1) today
  runStderrLoggingT $ filterLogger (\_ ll -> ll >= logLevel) $ withSqlitePoolInfo info 1 $ \pool -> do
    tokens <- liftIO $ runSqlPool collectOuraTokens pool
    forM_ tokens $ \(userId, token) -> do
      scores <- liftIO $ runClientM' clientEnv $ collectScores lastMonth today userId token
      liftIO $ runSqlPool (insertScores scores) pool
  where
    logLevel = LevelWarn
