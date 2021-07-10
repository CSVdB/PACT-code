module PACT.Data.Email where

import Prelude
import Data.Argonaut as A
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Profunctor (wrapIso)
import Data.Newtype (class Newtype)

newtype EmailAddress = EmailAddress String

derive instance Newtype EmailAddress _
derive instance Eq EmailAddress
derive instance Ord EmailAddress

derive newtype instance Show EmailAddress

instance A.EncodeJson EmailAddress where
  encodeJson (EmailAddress s) = A.fromString s

instance A.DecodeJson EmailAddress where
  decodeJson = map EmailAddress <<< decodeString
