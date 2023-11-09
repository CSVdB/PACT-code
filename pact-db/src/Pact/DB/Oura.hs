{-# LANGUAGE TupleSections #-}

module Pact.DB.Oura where

import Control.Monad.IO.Class
import Data.Functor
import Data.List (intersect)
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Database.Persist.Sqlite
import Pact.DB
import Pact.Data

type OverallScores = [DailyScore]

combineScores :: UserUUID -> ActivityData -> SleepData -> ReadinessData -> OverallScores
combineScores userId (ActivityData amap) (SleepData smap) (ReadinessData rmap) =
  keys <&> \k ->
    DailyScore -- We can use (!) because the keys are in the intersection of the maps
      { dailyScoreActivity = amap Map.! k,
        dailyScoreSleep = smap Map.! k,
        dailyScoreReadiness = rmap Map.! k,
        dailyScoreDay = k,
        dailyScoreUser = userId
      }
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

insertScores :: MonadIO m => OverallScores -> SqlPersistT m ()
insertScores = mapM_ insert_

fetchYesterdaysScore :: MonadIO m => UserUUID -> SqlPersistT m (Maybe DailyScore)
fetchYesterdaysScore userId = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let yesterday = addDays (-1) today
  entityVal <$$> selectFirst [DailyScoreUser ==. userId, DailyScoreDay ==. yesterday] []
