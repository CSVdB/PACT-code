{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.User where

import Data.Password.Bcrypt
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import Numeric.Natural
import Servant.API.Generic
import Text.Blaze
import YamlParse.Applicative

data RegisterForm = RegisterForm
  { registerFormUsername :: Username,
    registerFormPassword :: Password,
    registerFormConfirmPassword :: Password
  }
  deriving (Show, Generic)

instance Validity Password where
  validate = trivialValidation

confirmPasswords :: RegisterForm -> Bool
confirmPasswords RegisterForm {..} = unsafeShowPassword registerFormPassword == unsafeShowPassword registerFormConfirmPassword

instance Validity RegisterForm where
  validate rf@RegisterForm {..} =
    mconcat
      [ genericValidate rf,
        declare "ConfirmPassword confirms the password" $ confirmPasswords rf
      ]

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Password
  }
  deriving (Show, Generic)

instance Validity LoginForm

newtype Username = Username
  { usernameText :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToMarkup Username where
  toMarkup = toMarkup . usernameText

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
  fromPersistValue (PersistText t) = parseUsername t
  fromPersistValue _ = Left "Not text"

instance PersistFieldSql Username where
  sqlType _ = SqlString

instance YamlSchema Username where
  yamlSchema = eitherParser parseUsernameOrErr yamlSchema

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

parseUsername :: Text -> Either Text Username
parseUsername = mapLeft T.pack . prettyValidate . Username

parseUsernameOrErr :: Text -> Either String Username
parseUsernameOrErr = prettyValidate . Username

newtype Coins = Coins
  { unCoins :: Natural
  }
  deriving newtype (Num)
  deriving (Show, Eq, Ord, Generic)

instance Validity Coins where
  validate coins =
    mconcat
      [ genericValidate coins,
        declare "coins >= 0" $ unCoins coins >= 0
      ]

instance PersistField Coins where
  toPersistValue (Coins n) = toPersistValue (fromIntegral n :: Int)
  fromPersistValue v = Coins . fromIntegral <$> (fromPersistValue v :: Either Text Int)

instance PersistFieldSql Coins where
  sqlType _ = sqlType $ Proxy @Int
