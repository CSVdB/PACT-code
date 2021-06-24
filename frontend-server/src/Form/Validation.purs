module PACT.Form.Validation where

import Prelude
import PACT.Data.Email (EmailAddress(..))
import PACT.Data.User (Username, mkUsername)
import Data.Either (Either(..), note)
import Data.String as String
import Formless as F

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidEmail
  | InvalidUsername

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidEmail -> "Invalid email address"
  InvalidUsername -> "Invalid username"

condition :: ∀ a. (a -> Boolean) -> FormError -> a -> Either FormError a
condition f err a = if f a then pure a else Left err

required :: ∀ form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ condition (_ /= mempty) Required

minLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
minLength n = F.hoistFnE_ $ condition (\str -> String.length str > n) TooShort

emailFormat :: ∀ form m. Monad m => F.Validation form m FormError String EmailAddress
emailFormat = F.hoistFnE_ $ map EmailAddress <<< condition (String.contains (String.Pattern "@")) InvalidEmail

usernameFormat :: ∀ form m. Monad m => F.Validation form m FormError String Username
usernameFormat = F.hoistFnE_ $ note InvalidUsername <<< mkUsername
