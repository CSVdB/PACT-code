{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.Exercise where

import Data.Aeson
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.UUID ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Servant.Client.Core (BaseUrl, parseBaseUrl, showBaseUrl)
import Text.Read (readEither)

type ExerciseUUID = UUID

instance PersistField UUID where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql UUID where
  sqlType _ = SqlString

data Difficulty = Easy | Medium | Hard deriving (Show, Eq, Ord, Generic, Read)

instance Validity Difficulty -- Any value is valid

instance FromJSON Difficulty

instance ToJSON Difficulty

instance PersistField Difficulty where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql Difficulty where
  sqlType _ = SqlString

data WorkoutType
  = Push
  | Pull
  | Core
  | Legs
  | FullBody
  | Cardio
  | HIIT
  deriving (Show, Eq, Ord, Generic)

data MuscleGroup
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
  | Lungs -- "Muscle group" for cardio workouts
  deriving (Show, Eq, Ord, Generic, Read)

instance Validity MuscleGroup -- Any value is valid

instance FromJSON MuscleGroup

instance ToJSON MuscleGroup

instance PersistField MuscleGroup where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql MuscleGroup where
  sqlType _ = SqlString

-- TODO: Serve all images and videos on our own website (e.g.
-- `static.pactcommunity.be`), generate a hash for each, and replace `BaseUrl`
-- by `hash + ".png"` or "hash + ".mp4"`.
type SourceURI = BaseUrl

instance Validity BaseUrl where
  validate baseUrl = case parseBaseUrl $ showBaseUrl baseUrl of
    Left err -> invalid $ show err
    Right _ -> valid

instance PersistField BaseUrl where
  toPersistValue = toPersistValue . showBaseUrl
  fromPersistValue (PersistText t) = case parseBaseUrl $ T.unpack t of
    Left err -> Left $ T.pack $ show err
    Right baseUrl -> Right baseUrl
  fromPersistValue _ = Left "No text"

instance PersistFieldSql BaseUrl where
  sqlType _ = SqlString

-- Type synonym as reminder that form tips should be printed as a list.
type FormTips = Text

data ExerciseMaterial
  = TRX
  | PullupBar
  | DipBars
  | ThingForInclined
  | Parallites
  deriving (Show, Eq, Ord, Generic, Read)

prettyPrint :: ExerciseMaterial -> String
prettyPrint TRX = "TRX"
prettyPrint PullupBar = "pullup bar"
prettyPrint DipBars = "dip bars"
prettyPrint ThingForInclined = "something for inclined exercises"
prettyPrint Parallites = "parallites"

instance Validity ExerciseMaterial

instance FromJSON ExerciseMaterial

instance ToJSON ExerciseMaterial

instance PersistField ExerciseMaterial where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql ExerciseMaterial where
  sqlType _ = SqlString

data Elastics
  = Possible
  | NotPossible
  deriving (Show, Eq, Ord, Generic, Read)

instance Validity Elastics

instance FromJSON Elastics

instance ToJSON Elastics

instance PersistField Elastics where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql Elastics where
  sqlType _ = SqlString
