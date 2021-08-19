{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.Exercise where

import Data.Aeson
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read (readEither)

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
  | HIIT
  | Lungs -- "Muscle group" for cardio workouts
  deriving (Show, Eq, Ord, Generic, Read, Enum, Bounded)

instance Validity Muscle -- Any value is valid

instance PersistField Muscle where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql Muscle where
  sqlType _ = SqlString
