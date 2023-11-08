{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Oura where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.Functor
import Data.List (intersect)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Vector as V
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import System.Exit (die)

-- TODO:
-- - Combine data into OverallScores
-- - Write tests
-- - Implement OverallScores in dB
-- - Push data into dB
-- - Test dB integration
-- - Insert OURA_TOKEN into dB
-- - Run script for all OURA_TOKENS
-- - Set up service to run script every day
-- - Include yesterday's scores in your profile
-- - Deploy & check if everything is working

type OuraToken = Text

type Score = Int -- Value between 0 and 100

newtype ActivityData = ActivityData (Map Day Score) deriving (Show)

getScore :: Value -> Parser (Day, Score)
getScore = withObject "score_element" $ \o -> do
  day <- o .: "day"
  score <- o .: "score"
  pure (day, score)

instance FromJSON ActivityData where
  parseJSON = withObject "activity_data" $ \o ->
    case KM.lookup "data" o of -- List of key-value maps
      Just (Array xs) -> ActivityData . Map.fromList <$> mapM getScore (V.toList xs)
      _ -> fail "The activity data didn't contain a list under the key \"data\""

newtype SleepData = SleepData (Map Day Score) deriving (Show)

instance FromJSON SleepData where
  parseJSON = withObject "sleep_data" $ \o ->
    case KM.lookup "data" o of -- List of key-value maps
      Just (Array xs) -> SleepData . Map.fromList <$> mapM getScore (V.toList xs)
      _ -> fail "The sleep data didn't contain a list under the key \"data\""

newtype ReadinessData = ReadinessData (Map Day Score) deriving (Show)

instance FromJSON ReadinessData where
  parseJSON = withObject "readiness_data" $ \o ->
    case KM.lookup "data" o of -- List of key-value maps
      Just (Array xs) -> ReadinessData . Map.fromList <$> mapM getScore (V.toList xs)
      _ -> fail "The readiness data didn't contain a list under the key \"data\""

data DailyScore = DailyScore
  { asActivity :: Score,
    asSleep :: Score,
    asReadiness :: Score
  }
  deriving (Show)

newtype OverallScores = OverallScores (Map Day DailyScore) deriving (Show)

combineScores :: ActivityData -> SleepData -> ReadinessData -> OverallScores
combineScores (ActivityData amap) (SleepData smap) (ReadinessData rmap) =
  OverallScores . Map.fromList $
    keys <&> \k ->
      let score =
            DailyScore -- We can use (!) because the keys are in the intersection of the maps
              { asActivity = amap Map.! k,
                asSleep = smap Map.! k,
                asReadiness = rmap Map.! k
              }
       in (k, score)
  where
    aKeys = Map.keys amap
    sKeys = Map.keys smap
    rKeys = Map.keys rmap
    keys = intersect aKeys $ intersect sKeys rKeys

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
