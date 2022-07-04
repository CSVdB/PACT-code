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
  = Acrogym
  | Calisthenics
  | Climbing
  | Cycling
  | Gym
  | HIIT
  | Jog
  | Swim
  | Walking
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

data UnitType = Minutes | Kilometers | Meters
  deriving (Show, Eq, Ord, Generic)

unitType :: WorkoutType -> UnitType
unitType Acrogym = Minutes
unitType Jog = Kilometers
unitType Calisthenics = Minutes
unitType Climbing = Minutes
unitType Cycling = Kilometers
unitType HIIT = Minutes
unitType Gym = Minutes
unitType Swim = Meters
unitType Walking = Kilometers

unitDecimals :: UnitType -> Int
unitDecimals Minutes = 0
unitDecimals Kilometers = 1
unitDecimals Meters = 0

amountMessage :: WorkoutType -> Text
amountMessage = amountMessage' . unitType

amountMessage' :: UnitType -> Text
amountMessage' Minutes = "minutes"
amountMessage' Kilometers = "km"
amountMessage' Meters = "m"

stepSize :: WorkoutType -> Double
stepSize = stepSize' . unitType

stepSize' :: UnitType -> Double
stepSize' Minutes = 1
stepSize' Kilometers = 0.1
stepSize' Meters = 1

myPrettyCfg :: Int -> PrettyCfg
myPrettyCfg n = PrettyCfg n (Just '\'') '.'

showWorkoutAmount :: WorkoutType -> WorkoutAmount -> Text
showWorkoutAmount workoutType (WorkoutAmount n) =
  prettyF (myPrettyCfg i) (fromIntegral n * stepSize workoutType) <> stepSizeText
  where
    i = unitDecimals $ unitType workoutType
    stepSizeText = amountMessage workoutType
