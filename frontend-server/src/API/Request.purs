module PACT.API.Request where

import Prelude hiding ((/))
import PACT.Data.User (LoginForm, Profile, RegistrationForm, loginFormCodec, profileCodec, registrationFormCodec)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Bifunctor (lmap)
import Data.HTTP.Method (Method(..))
import Data.Argonaut.Core (Json)
import Data.Generic.Rep (class Generic)
import Data.Codec as Codec
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut (printJsonDecodeError, JsonCodec)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (RouteDuplex', int, root, segment, print)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Affjax (Request, request, printError)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF

newtype BaseURL = BaseURL String

data Endpoint
  = Login
  | Register
  | Greet
  | Number Int

derive instance Generic Endpoint _

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Login": "login" / noArgs
  , "Register": "register" / noArgs
  , "Greet": "greet" / noArgs
  , "Number": "number" / int segment
  }

-- Implement creating asynchronous API requests to the backend.
defaultRequest :: BaseURL -> Endpoint -> RequestMethod -> Request Json
defaultRequest (BaseURL url) endpoint method =
    { content: RB.json <$> body
    , headers: [] -- TODO: Once auth is set up, put token here
    , method: Left requestMethod
    , password: Nothing
    , responseFormat: RF.json
    , timeout: Nothing -- TODO: Are you sure?
    , url: url <> print endpointCodec endpoint
    , username: Nothing
    , withCredentials: false
    }
  where
    Tuple requestMethod body = case method of
      Get -> Tuple GET Nothing
      Post b -> Tuple POST b
      Put b -> Tuple PUT b
      Delete -> Tuple DELETE Nothing

-- Actually execute the API call and get the result
apiRequest :: forall m a. MonadAff m => BaseURL -> Endpoint -> RequestMethod -> JsonCodec a -> m (Either String a)
apiRequest url endpoint method codec = do
    res <- liftAff <<< request $ defaultRequest url endpoint method
    case res of
        Left err -> pure <<< Left $ printError err
        Right resp -> pure <<< lmap printJsonDecodeError $ Codec.decode codec resp.body

login :: forall m. MonadAff m => BaseURL -> LoginForm -> m (Either String
Profile)
login url form = apiRequest url Login method profileCodec
  where
    method = Post <<< Just $ Codec.encode loginFormCodec form

register :: forall m. MonadAff m => BaseURL -> RegistrationForm -> m (Either String Profile)
register url form = apiRequest url Register method profileCodec
  where
    method = Post $ Just $ Codec.encode registrationFormCodec form

greet :: forall m. MonadAff m => BaseURL -> m (Either String String)
greet url = apiRequest url Greet Get CA.string
