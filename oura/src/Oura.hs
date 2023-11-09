{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Oura where

import Data.Proxy
import Data.Time.Calendar
import Data.Time.Clock
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Pact.DB.Oura
import Pact.Data.Oura
import Servant.API
import Servant.Client
import System.Exit (die)

-- TODO:
-- - Implement OverallScores in dB
-- - Push data into dB
-- - Test dB integration
-- - Insert OURA_TOKEN into dB
-- - Run script for all OURA_TOKENS
-- - Set up service to run script every day
-- - Include yesterday's scores in your profile
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
  activityData <- runClientM' clientEnv $ clientCollectActivityData lastMonth today token
  sleepData <- runClientM' clientEnv $ clientCollectSleepData lastMonth today token
  readinessData <- runClientM' clientEnv $ clientCollectReadinessData lastMonth today token
  let scores = combineScores activityData sleepData readinessData
  print scores
  where
    token = "Bearer 2G32LBAM3Q2ZN6TRERAK4MTWMON6AOGQ"
