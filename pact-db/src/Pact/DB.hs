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

import Control.Monad
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Password.Bcrypt
import Data.Password.Instances ()
import Data.Text (Text)
import qualified Data.Text as T
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
  exercise ExerciseUUID
  muscle Muscle

  deriving Show Eq Ord Generic

ExerciseMaterial
  uuid ExerciseMaterialUUID
  name Text

  UniqueMaterialUUID uuid
  UniqueMaterialName name

  deriving Show Eq Ord Generic

MaterialFilter
  exercise ExerciseUUID
  material ExerciseMaterialUUID

  deriving Show Eq Ord Generic

ExerciseAlternativeName
  uuid ExerciseUUID
  name Text

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

instance Validity MuscleFilter

instance Validity ExerciseMaterial where
  validate material@ExerciseMaterial {..} =
    mconcat
      [ genericValidate material, -- Actually only valid if present in the dB
        declare "Name is not empty" $ not $ T.null exerciseMaterialName
      ]

instance Validity MaterialFilter

instance Validity ExerciseAlternativeName where
  validate name@ExerciseAlternativeName {..} =
    mconcat
      [ genericValidate name,
        declare "Name is not empty" $ not $ T.null exerciseAlternativeNameName
      ]

newtype Material = Material {unMaterial :: Text}
  deriving (Show, Eq, Ord, Generic)

newtype AlternativeName = AlternativeName {unAlternative :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity AlternativeName where
  validate name =
    mconcat
      [ genericValidate name,
        declare "Name is not empty" . not . T.null $ unAlternative name
      ]

data CompleteExercise = CompleteExercise
  { exerciseCE :: Exercise,
    musclesCE :: [Muscle],
    materialsCE :: [Material],
    altNamesCE :: [AlternativeName]
  }
  deriving (Show, Eq, Ord, Generic)

collectExercise ::
  MonadIO m =>
  ExerciseUUID ->
  SqlPersistT m (Maybe CompleteExercise)
collectExercise uuid = do
  res <- getBy $ UniqueExerciseUUID uuid
  case res of
    Nothing -> pure Nothing
    Just (Entity _ ex@Exercise {..}) -> do
      muscleFilters <- selectList [MuscleFilterExercise ==. exerciseUuid] []
      let muscles = muscleFilterMuscle . entityVal <$> muscleFilters
      materials <- collectMaterials uuid
      names <- collectAltNames uuid
      pure . Just $
        CompleteExercise
          { exerciseCE = ex,
            musclesCE = muscles,
            materialsCE = materials,
            altNamesCE = names
          }

collectMaterials :: MonadIO m => ExerciseUUID -> SqlPersistT m [Material]
collectMaterials uuid = do
  materialFilters <- selectList [MaterialFilterExercise ==. uuid] []
  let materialUuids = materialFilterMaterial . entityVal <$> materialFilters
  materials <- forM materialUuids $ \mUuid -> do
    res <- getBy $ UniqueMaterialUUID mUuid
    pure $ case res of
      Nothing -> Nothing
      Just (Entity _ material) -> Just material
  pure $ Material . exerciseMaterialName <$> catMaybes materials

collectAltNames :: MonadIO m => ExerciseUUID -> SqlPersistT m [AlternativeName]
collectAltNames uuid =
  fmap toAltName <$> selectList [ExerciseAlternativeNameUuid ==. uuid] []
  where
    toAltName = AlternativeName . exerciseAlternativeNameName . entityVal

collectAllMaterials :: MonadIO m => SqlPersistT m [ExerciseMaterial]
collectAllMaterials = fmap entityVal <$> selectList [] []

alternativeNamesText :: [AlternativeName] -> Text
alternativeNamesText = T.intercalate ", " . fmap unAlternative

altNames :: Text -> [AlternativeName]
altNames t = AlternativeName <$> T.splitOn ", " t
