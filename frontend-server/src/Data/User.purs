module PACT.Data.User
  ( Username
  , mkUsername
  , toString
  , usernameCodec
  , Profile(..)
  , profileCodec
  , InitialForm
  , LoginForm
  , loginFormCodec
  , RegistrationForm
  , registrationFormCodec
  ) where

import Prelude
import PACT.Data.Email (EmailAddress, emailAddressCodec)
import Data.Maybe (Maybe(..))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (dimap)

newtype Username = Username String

derive instance Eq Username
derive instance Ord Username

mkUsername :: String -> Maybe Username
mkUsername "" = Nothing
mkUsername s = Just $ Username s

toString :: Username -> String
toString (Username n) = n

-- Implementing `FromJSON` and `ToJSON` in one go
usernameCodec :: JsonCodec Username
usernameCodec = dimap (\(Username n) -> n) Username CA.string

type Profile = 
  { username :: Username
  , email :: EmailAddress
  }

profileCodec :: JsonCodec Profile
profileCodec = CAR.object "Profile"
  { username: usernameCodec
  , email: emailAddressCodec
  }

type InitialForm row =
  ( email :: EmailAddress
  , password :: String
  | row
  )

type LoginForm = { | InitialForm () }

loginFormCodec :: JsonCodec LoginForm
loginFormCodec = CAR.object "LoginForm"
  { email: emailAddressCodec
  , password: CA.string
  }

type RegistrationForm = { | InitialForm (username :: Username) }

registrationFormCodec :: JsonCodec RegistrationForm
registrationFormCodec = CAR.object "RegistrationForm"
  { username: usernameCodec
  , email: emailAddressCodec
  , password: CA.string
  }
