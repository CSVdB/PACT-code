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

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Password.Bcrypt
import Data.Password.Instances ()
import Data.Text (Text)
import Data.Validity
import Data.Validity.Persist ()
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Pact.Data

share
  [mkPersist sqlSettings, mkMigrate "serverMigration"]
  [persistLowerCase|

User
  name Username
  password (PasswordHash Bcrypt)
  email EmailAddress

  UniqueUsername name

  deriving Show Eq Ord Generic

MyRandomInt
  myRandomTypeInt Int

  deriving Show Eq Ord Generic

Exercise
  title Text
  easier [Exercise] -- If exercise is too hard, replace by these
  difficulty Difficulty
  muscleGroups [MuscleGroup]
  formTips [FormTip]
  videos [SourceURI] -- For now, this is a URL from the internet.
    -- Eventually, a relative path where the video is deployed on our platform.
  images [SourceURI] -- Dito

  deriving Show Eq Ord Generic
|]

instance Validity (Salt a) where
  validate = trivialValidation

instance Validity Password where
  validate = trivialValidation

instance Validity (PasswordHash a) where
  validate = trivialValidation

instance Validity User

toProfile :: User -> Profile
toProfile User {..} = Profile {profileName = userName, profileEmail = userEmail}

registrationToUser :: MonadIO m => RegistrationForm -> m User
registrationToUser RegistrationForm {..} = do
  pass <- hashPassword $ mkPassword registrationFormPassword
  pure $
    User
      { userName = registrationFormUsername,
        userPassword = pass,
        userEmail = registrationFormEmail
      }

instance Validity Exercise -- Any value with well-formed Texts and such, is valid

instance FromJSON Exercise where
  parseJSON = withObject "Exercise" $ \o ->
    Exercise <$> o .: "title"
      <*> o .: "easierExercises"
      <*> o .: "difficulty"
      <*> o .: "muscleGroups"
      <*> o .: "formTips"
      <*> o .: "videos"
      <*> o .: "images"

instance ToJSON Exercise where
  toJSON Exercise {..} =
    object
      [ "title" .= exerciseTitle,
        "easierExercises" .= exerciseEasier,
        "difficulty" .= exerciseDifficulty,
        "muscleGroups" .= exerciseMuscleGroups,
        "formTips" .= exerciseFormTips,
        "videos" .= exerciseVideos,
        "images" .= exerciseImages
      ]
