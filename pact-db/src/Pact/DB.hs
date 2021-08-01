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

Exercise
  name Text
  alternativeNames Text -- List of names as comma-separated values
  easier [Text] -- If exercise is too hard, replace by these
  difficulty Difficulty
  muscleGroups [MuscleGroup]
  formTips FormTips
  video SourceURI Maybe -- For now, this is a URL from the internet.
    -- Eventually, a relative path where the video is deployed on our platform.
  images [SourceURI] -- Dito
  material ExerciseMaterial Maybe
  elasticsPossible Elastics
  notes Text

  UniqueExerciseName name

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
