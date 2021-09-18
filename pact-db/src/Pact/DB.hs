{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Time.Calendar
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
  uuid UserUUID
  name Username
  password (PasswordHash Bcrypt)

  UniqueUserUUID uuid
  UniqueUsername name

  deriving Show Eq Ord Generic

Coach -- A coach is an extended version of a user
  user UserUUID
  uuid CoachUUID
  pic ImageUUID Maybe
  aboutMe Textarea

  UniqueCoach uuid
  UniqueCoachUser user

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

CustomerCoachRelation
  customer UserUUID
  coach CoachUUID
  response ProposalResponse Maybe

  UniqueRelation customer coach

  deriving Show Eq Ord Generic

UserWorkout
  user UserUUID
  type WorkoutType
  day Day
  amount WorkoutAmount

  deriving Show Eq Ord Generic

CoachWorkout
  coach CoachUUID
  type WorkoutType
  day Day
  amount WorkoutAmount
  notes Textarea

  deriving Show Eq Ord Generic
|]

instance Validity (Salt a) where
  validate = trivialValidation

instance Validity (PasswordHash a) where
  validate = trivialValidation

instance Validity User

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

collectCustomerCoachProposals :: MonadIO m => Coach -> SqlPersistT m [User]
collectCustomerCoachProposals Coach {..} = do
  entities <- selectList [CustomerCoachRelationCoach ==. coachUuid, CustomerCoachRelationResponse ==. Nothing] []
  let userIds = customerCoachRelationCustomer . entityVal <$> entities
  fmap catMaybes $ forM userIds $ fmap (fmap entityVal) . getBy . UniqueUserUUID

data SqlUpdateResult
  = SqlSuccess
  | SqlNotFound
  | SqlAlreadyUpdated
  deriving (Show, Eq, Ord, Generic)

-- Respond to a customer coach proposal.
respondToProposal ::
  MonadIO m =>
  UserUUID ->
  CoachUUID ->
  ProposalResponse ->
  SqlPersistT m SqlUpdateResult
respondToProposal user coach response =
  getBy (UniqueRelation user coach) >>= \case
    Nothing -> pure SqlNotFound
    Just (Entity key val) -> do
      case customerCoachRelationResponse val of
        Nothing -> do
          update key [CustomerCoachRelationResponse =. Just response]
          pure SqlSuccess
        Just _ -> pure SqlAlreadyUpdated

tuple :: a -> b -> (a, b)
tuple a b = (a, b)

-- TODO: Once the concept of friends is introduced, this should list all
-- workouts done in the last week by a friend of the given user.
getLastWeeksWorkouts :: MonadIO m => User -> SqlPersistT m [(User, UserWorkout)]
getLastWeeksWorkouts user@User {..} =
  fmap (tuple user . entityVal) <$> selectList [UserWorkoutUser ==. userUuid] []
