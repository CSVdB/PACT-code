{-# LANGUAGE TupleSections #-}

module Pact.DB.Oura where

import Control.Monad.IO.Class
import Data.Functor
import Data.List (intersect)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Calendar
import Database.Persist.Sqlite
import Pact.DB
import Pact.Data

newtype OverallScores = OverallScores (Map Day DailyScore) deriving (Show)

combineScores :: ActivityData -> SleepData -> ReadinessData -> OverallScores
combineScores (ActivityData amap) (SleepData smap) (ReadinessData rmap) =
  OverallScores . Map.fromList $
    keys <&> \k ->
      let score =
            DailyScore -- We can use (!) because the keys are in the intersection of the maps
              { dailyScoreActivity = amap Map.! k,
                dailyScoreSleep = smap Map.! k,
                dailyScoreReadiness = rmap Map.! k
              }
       in (k, score)
  where
    aKeys = Map.keys amap
    sKeys = Map.keys smap
    rKeys = Map.keys rmap
    keys = intersect aKeys $ intersect sKeys rKeys

collectOuraTokens :: MonadIO m => SqlPersistT m [(UserUUID, OuraToken)]
collectOuraTokens = do
  users <- selectListVals [] [] -- TODO: Filters out the users without OuraTokens right away
  let maybeUsers = users <&> \user -> (userUuid user,) <$> userOuraToken user
  pure $ catMaybes maybeUsers
