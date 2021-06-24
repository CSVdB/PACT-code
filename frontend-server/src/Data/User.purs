module PACT.Data.User
  ( Username
  , mkUsername
  , toString
  , usernameCodec
  , Profile(..)
  , profileCodec
  , InitialForm
  , Password
  , LoginFields
  , loginFieldsCodec
  , RegisterFields
  , registerFieldsCodec
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
usernameCodec = dimap toString Username CA.string

type Profile = 
  { username :: Username
  , email :: EmailAddress
  }

profileCodec :: JsonCodec Profile
profileCodec = CAR.object "Profile"
  { username: usernameCodec
  , email: emailAddressCodec
  }

type Password = String

type InitialForm row =
  ( username :: Username
  , password :: Password
  | row
  )

type LoginFields = { | InitialForm () }

loginFieldsCodec :: JsonCodec LoginFields
loginFieldsCodec = CAR.object "LoginFields"
  { username: usernameCodec
  , password: CA.string
  }

type RegisterFields = { | InitialForm (email :: EmailAddress) }

registerFieldsCodec :: JsonCodec RegisterFields
registerFieldsCodec = CAR.object "RegisterFields"
  { username: usernameCodec
  , email: emailAddressCodec
  , password: CA.string
  }
