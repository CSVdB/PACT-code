{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.DB where

import Data.ByteString (ByteString)
import Data.Password.Bcrypt
import Data.Password.Instances ()
import Data.Text (Text)
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Persist ()
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Pact.Data
import Yesod

share
  [mkPersist sqlSettings, mkMigrate "serverMigration"]
  [persistLowerCase|

User
  name Username
  password (PasswordHash Bcrypt)

  UniqueUsername name

  deriving Show Eq Ord Generic

Exercise
  uuid ExerciseUUID
  image ImageUUID
  video VideoUUID
  name Text
  difficulty Difficulty
  formTips FormTips
  notes Text

  UniqueExerciseUUID uuid
  UniqueExerciseImageUUID image
  UniqueExerciseName name

  deriving Show Eq Ord Generic

Image
  uuid ImageUUID -- TODO: Replace this by a key based on the hash of the contents and type
  contents ByteString
  typ Text -- Content type

  UniqueImageUUID uuid

  deriving Show Eq Ord Generic

Video
  uuid VideoUUID -- TODO: Replace this by a key based on the hash of the contents and type
  contents ByteString
  typ Text -- Content type

  UniqueVideoUUID uuid

  deriving Show Eq Ord Generic

MuscleFilter
  exerciseUuid ExerciseUUID
  muscle Muscle

  deriving Show Eq Ord Generic
|]

instance Validity (Salt a) where
  validate = trivialValidation

instance Validity (PasswordHash a) where
  validate = trivialValidation

instance Validity User

toProfile :: User -> Profile
toProfile User {..} = Profile {profileName = userName}

instance Validity Exercise -- Any value with well-formed Texts and such, is valid

instance Validity Image

instance Validity Video

collectExercise :: MonadIO m => ExerciseUUID -> SqlPersistT m (Maybe (Exercise, [Muscle]))
collectExercise uuid = do
  res <- getBy $ UniqueExerciseUUID uuid
  case res of
    Nothing -> pure Nothing
    Just (Entity _ ex@Exercise {..}) -> do
      filters <- selectList [MuscleFilterExerciseUuid ==. exerciseUuid] []
      let muscles = muscleFilterMuscle . entityVal <$> filters
      pure $ Just (ex, muscles)
