module PACT.Form.Validation where

import Prelude
import PACT.Data.Email (EmailAddress(..))
import PACT.Data.User (Username, mkUsername, Password)
import Data.Either (Either(..), note)
import Data.String as String
import Data.Natural (Natural, natToInt, intToNat)
import Formless as F

data FormError
  = Required
  | TooShort String Natural
  | TooLong
  | InvalidEmail
  | InvalidUsername
  | PasswordsUnequal

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort topic n -> topic <> " requires at least " <> show n <> " characters."
  TooLong -> "Too many characters entered"
  InvalidEmail -> "Invalid email address"
  InvalidUsername -> "Invalid username"
  PasswordsUnequal -> "Passwords unequal"

condition :: ∀ a. (a -> Boolean) -> FormError -> a -> Either FormError a
condition f err a = if f a then pure a else Left err

required :: ∀ form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ condition (_ /= mempty) Required

minLength :: ∀ form m. Monad m => String -> Natural -> F.Validation form m FormError String String
minLength topic n = F.hoistFnE_ $ lengthCondition $ TooShort topic n
  where
    lengthCondition = condition $ \str -> String.length str >= natToInt n

usernameValidator ::
  forall form m.
  Monad m =>
  F.Validation form m FormError String Username
usernameValidator = required >>> usernameFormat
  where
    usernameFormat = F.hoistFnE_ $ note InvalidUsername <<< mkUsername

passwordValidator ::
  forall form m.
  Monad m =>
  F.Validation form m FormError String Password
passwordValidator = required >>> minLength "Password" (intToNat 8)

emailValidator ::
  forall form m.
  Monad m => F.Validation form  m FormError String EmailAddress
emailValidator = required >>> minLength "Email address" (intToNat 3) >>> emailFormat
  where
    isEmail = String.contains $ String.Pattern "@"
    emailFormat = F.hoistFnE_ $ map EmailAddress <<< condition isEmail InvalidEmail
