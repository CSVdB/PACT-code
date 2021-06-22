{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.API.Server.DB where

import Control.Monad.IO.Class (MonadIO)
import Data.Password
import Data.Password.Bcrypt
import Data.Password.Instances ()
import Data.Validity
import Data.Validity.Persist ()
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Pact.API.Data

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
