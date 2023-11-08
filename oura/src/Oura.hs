module Oura where

import Data.Time.Calendar (Day)
import Servant.API

-- TODO:
-- - Implement ActivityData, SleepData and ReadinessData: Day & Score
-- - Generate a client for OuraAPI, pointed to https://api.ouraring.com, and call one endpoint with the executable. Print dates and scores to verify everything works.
-- - Implement CLI to call all endpoints
-- - Test the CLI
-- - Clean up devShell code

type OuraAccessToken = Text

type ActivityData = Text

type SleepData = Text

type ReadinessData = Text

type OuraAPI = "v2" :> "usercollection" :> (ActivityEndpoint :<|> SleepEndpoint :<|> ReadinessEndpoint)

type ActivityEndpoint = "daily_activity" :> QueryParams "start_date" Day :> QueryParams "end_date" Day :> QueryParams "access_token" OuraAccessToken :> GET ['JSON] [ActivityData]

type SleepEndpoint = "daily_sleep" :> QueryParams "start_date" Day :> QueryParams "end_date" Day :> QueryParams "access_token" OuraAccessToken :> GET ['JSON] [SleepData]

type ReadinessEndpoint = "daily_readiness" :> QueryParams "start_date" Day :> QueryParams "end_date" Day :> QueryParams "access_token" OuraAccessToken :> GET ['JSON] [ReadinessData]

ouraSync :: IO ()
ouraSync = putStrLn "Not implemented yet"
