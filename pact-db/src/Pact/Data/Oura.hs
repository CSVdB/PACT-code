{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Data.Oura where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Calendar
import qualified Data.Vector as V

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
