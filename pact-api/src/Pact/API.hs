{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Pact.API where

import Data.Proxy
import Data.Text (Text)
import Data.Validity.Text ()
import Pact.API.Data
import Servant.API
import Servant.API.Generic
import Servant.Auth

pactAPI :: Proxy PactAPI
pactAPI = Proxy

type PactAPI = ToServantApi PactRoutes

data PactRoutes route = PactRoutes
  { postRegister :: !(route :- PostRegister),
    postLogin :: !(route :- PostLogin),
    getGreet :: !(route :- GetGreet)
  }
  deriving (Generic)

type PostRegister =
  "register" :> ReqBody '[JSON] RegistrationForm :> Post '[JSON] NoContent

type PostLogin =
  "login" :> ReqBody '[JSON] LoginForm :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type ProtectAPI = Auth '[JWT] AuthCookie

type GetGreet = ProtectAPI :> "greet" :> Get '[JSON] Text
