{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.API.Data where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import Servant.API.Generic
import Servant.Auth.Server
import YamlParse.Applicative

newtype EmailAddress = EmailAddress String deriving (Show, Eq, Ord, Generic)

instance Validity EmailAddress

instance ToJSON EmailAddress where
  toJSON (EmailAddress s) = String $ T.pack s

instance FromJSON EmailAddress where
  parseJSON = withText "EmailAddress" $ pure . EmailAddress . T.unpack

instance PersistField EmailAddress where
  toPersistValue (EmailAddress s) = toPersistValue s
  fromPersistValue v = EmailAddress <$> fromPersistValue v

instance PersistFieldSql EmailAddress where
  sqlType _ = SqlString

data RegistrationForm = RegistrationForm
  { registrationFormUsername :: Username,
    registrationFormPassword :: Text,
    registrationFormEmail :: EmailAddress
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity RegistrationForm where
  validate rf@RegistrationForm {..} =
    mconcat
      [ genericValidate rf,
        declare "The password is nonempty" $
          not $
            T.null registrationFormPassword
      ]

instance ToJSON RegistrationForm where
  toJSON RegistrationForm {..} =
    object
      [ "username" .= registrationFormUsername,
        "password" .= registrationFormPassword,
        "email" .= registrationFormEmail
      ]

instance FromJSON RegistrationForm where
  parseJSON = withObject "RegistrationForm" $ \o ->
    RegistrationForm <$> o .: "username" <*> o .: "password" <*> o .: "email"

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity LoginForm

instance ToJSON LoginForm where
  toJSON LoginForm {..} =
    object
      ["username" .= loginFormUsername, "password" .= loginFormPassword]

instance FromJSON LoginForm where
  parseJSON = withObject "LoginForm" $ \o ->
    LoginForm <$> o .: "username" <*> o .: "password"

data AuthCookie = AuthCookie
  { authCookieUsername :: Username
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

newtype Username = Username
  { usernameText :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check
          (T.length t >= 3)
          "The username is at least three characters long."
      ]

instance PersistField Username where
  toPersistValue (Username t) = PersistText t
  fromPersistValue (PersistText t) =
    case parseUsername t of
      Nothing -> Left "Text isn't a valid username"
      Just un -> Right un
  fromPersistValue _ = Left "Not text"

instance PersistFieldSql Username where
  sqlType _ = SqlString

instance YamlSchema Username where
  yamlSchema = eitherParser parseUsernameOrErr yamlSchema

instance FromJSON Username where
  parseJSON = withText "Username" $ pure . Username

instance ToJSON Username where
  toJSON (Username t) = String t

parseUsername :: Text -> Maybe Username
parseUsername = constructValid . Username

parseUsernameOrErr :: Text -> Either String Username
parseUsernameOrErr = prettyValidate . Username

data Profile = Profile
  { profileName :: Username,
    profileEmail :: EmailAddress
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Profile

instance ToJSON Profile where
  toJSON Profile {..} =
    object
      [ "username" .= profileName,
        "email" .= profileEmail
      ]

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \o ->
    Profile <$> o .: "username" <*> o .: "email"
