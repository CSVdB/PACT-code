{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Pact.API where

import Data.Proxy
import Data.Text (Text)
import Data.Validity.Text ()
import Pact.Data
import Servant.API
import Servant.API.Generic
import Servant.Auth

pactAPI :: Proxy PactAPI
pactAPI = Proxy

type PactAPI = ToServantApi PactRoutes

data PactRoutes route = PactRoutes
  { postRegister :: !(route :- PostRegister),
    postLogin :: !(route :- PostLogin),
    getGreet :: !(route :- GetGreet),
    postNumber :: !(route :- PostNumber),
    getUser :: !(route :- GetUser)
  }
  deriving (Generic)

type PostRegister =
  "register" :> ReqBody '[JSON] RegistrationForm
    :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] Profile)

type PostLogin =
  "login" :> ReqBody '[JSON] LoginForm
    :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] Profile)

type ProtectAPI = Auth '[JWT] AuthCookie

type GetGreet = ProtectAPI :> "greet" :> Get '[JSON] Text

type PostNumber = ProtectAPI :> "number" :> Capture "number" Int :> Post '[JSON] NoContent

type GetUser = ProtectAPI :> "user" :> Get '[JSON] Profile
