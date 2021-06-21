module PACT.Data.Email where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Profunctor (wrapIso)
import Data.Newtype (class Newtype)

newtype EmailAddress = EmailAddress String

derive instance newtypeEmail :: Newtype EmailAddress _
derive instance Eq EmailAddress
derive instance Ord EmailAddress

emailAddressCodec :: JsonCodec EmailAddress
emailAddressCodec = wrapIso EmailAddress CA.string
