module PACT.Data.User
  ( Username
  , mkUsername
  , toString
  , Profile(..)
  , InitialForm
  , Password
  , LoginFields
  , RegisterFields
  ) where

import Prelude
import PACT.Data.Email (EmailAddress)
import Data.Maybe (Maybe(..))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (dimap)
import Data.Argonaut as A
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Encode.Encoders (encodeString)

newtype Username = Username String

derive instance Eq Username
derive instance Ord Username

derive newtype instance Show Username

mkUsername :: String -> Maybe Username
mkUsername "" = Nothing
mkUsername s = Just $ Username s

toString :: Username -> String
toString (Username n) = n

instance A.EncodeJson Username where
  encodeJson (Username s) = encodeString s

instance A.DecodeJson Username where
  decodeJson = map Username <<< decodeString

type Profile = 
  { username :: Username
  , email :: EmailAddress
  }

type Password = String

type InitialForm row =
  ( username :: Username
  , password :: Password
  | row
  )

type LoginFields = { | InitialForm () }

type RegisterFields = { | InitialForm (email :: EmailAddress) }
