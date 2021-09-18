{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.Exercise where

import Data.Aeson
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Format.Numbers
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read (readEither, readMaybe)
import Yesod

data Difficulty
  = Easy
  | Medium
  | Hard
  deriving (Show, Eq, Ord, Generic, Read, Enum, Bounded)

instance Validity Difficulty -- Any value is valid

instance FromJSON Difficulty

instance ToJSON Difficulty

instance PersistField Difficulty where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql Difficulty where
  sqlType _ = SqlString

type FormTips = Text

data Muscle
  = Shoulders
  | Abs
  | Biceps
  | Triceps
  | Forearms
  | Quads
  | Calves
  | Hamstrings
  | UpperChest
  | LowerChest
  | UpperBack
  | LowerBack
  | Heart -- "Muscle group" for HIIT training
  | Lungs -- "Muscle group" for cardio workouts
  deriving (Show, Eq, Ord, Generic, Read, Enum, Bounded)

instance Validity Muscle -- Any value is valid

instance PersistField Muscle where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql Muscle where
  sqlType _ = SqlString

data WorkoutType
  = Jog
  | Swim
  | Calisthenics
  | HIIT
  deriving (Show, Eq, Ord, Generic, Read, Enum, Bounded)

instance Validity WorkoutType -- Any value is valid

instance PersistField WorkoutType where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql WorkoutType where
  sqlType _ = SqlString

instance PathPiece WorkoutType where
  fromPathPiece = readMaybe . T.unpack
  toPathPiece = T.pack . show

newtype WorkoutAmount = WorkoutAmount {unWorkoutAmount :: Int}
  deriving (Show, Eq, Ord, Generic, Read, PersistField, PersistFieldSql)

instance Validity WorkoutAmount

-- Note: We explicitly write all WorkoutType cases here, instead of setting
-- `amountMessage _ = "minutes"`, to be safe against future WorkoutTypes.
amountMessage :: WorkoutType -> String
amountMessage Jog = "km"
amountMessage Swim = "m"
amountMessage Calisthenics = "minutes"
amountMessage HIIT = "minutes"

stepSize :: WorkoutType -> Double
stepSize Jog = 0.1
stepSize Swim = 1
stepSize Calisthenics = 1
stepSize HIIT = 1

myPrettyCfg :: Int -> PrettyCfg
myPrettyCfg n = PrettyCfg n (Just '\'') '.'

showWorkoutAmount' :: Int -> Text -> WorkoutType -> WorkoutAmount -> Text
showWorkoutAmount' i stepSizeText workoutType (WorkoutAmount n) =
  prettyF (myPrettyCfg i) (fromIntegral n * stepSize workoutType) <> stepSizeText

showWorkoutAmount :: WorkoutType -> WorkoutAmount -> Text
showWorkoutAmount workoutType amount = showWorkoutAmount' i stepSizeText workoutType amount
  where
    -- Note: We explicitly write all WorkoutType cases here, instead of setting
    -- "_ -> "(0, minutes)", to be safe against future WorkoutTypes.
    (i, stepSizeText) = case workoutType of
      Jog -> (1, "km")
      Swim -> (0, "m")
      Calisthenics -> (0, "minutes")
      HIIT -> (0, "minutes")
