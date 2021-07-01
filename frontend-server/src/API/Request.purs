module PACT.API.Request where

import Prelude hiding ((/))
import PACT.Data.User (LoginFields, Profile, RegisterFields, loginFieldsCodec, profileCodec, registerFieldsCodec)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), hush)
import Data.Tuple (Tuple(..))
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.HTTP.Method (Method(..))
import Data.Argonaut.Core (Json)
import Data.Generic.Rep (class Generic)
import Data.Codec as Codec
import Data.Codec.Argonaut (printJsonDecodeError, JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (RouteDuplex', int, root, segment, print)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Affjax (Request, request, printError)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseHeader (value)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- Represent JWT token used for authentication.
newtype Token
  = Token String

derive instance Eq Token
derive instance Ord Token

instance Show Token where
  show (Token _) = "Token {- token -}"

newtype BaseURL
  = BaseURL String

data Endpoint
  = Login
  | Register
  | User -- Get current user's profile (if authenticated)
  | Greet
  | Number Int

derive instance Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root
    $ sum
        { "Login": "login" / noArgs
        , "Register": "register" / noArgs
        , "User": "user" / noArgs
        , "Greet": "greet" / noArgs
        , "Number": "number" / int segment
        }

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

-- Implement creating asynchronous API requests to the backend.
defaultRequest :: BaseURL -> Maybe Token -> Endpoint -> RequestMethod -> Request Json
defaultRequest (BaseURL url) auth endpoint method =
  { content: RB.json <$> body
  , headers: case auth of
      Nothing -> []
      Just (Token t) -> [ RequestHeader "Authorization" $ "Token " <> t ]
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

-- Actually execute the API call and decode the result
apiRequest :: forall m a. MonadAff m
  => BaseURL -> Endpoint -> RequestMethod -> JsonCodec a -> m (Either String a)
apiRequest url endpoint method codec = do
  res <- liftAff <<< request $ defaultRequest url Nothing endpoint method
  pure $ case res of
    Left err -> Left $ printError err
    Right resp -> lmap printJsonDecodeError $ Codec.decode codec resp.body

apiRequestWithToken :: forall m a. MonadAff m
  => BaseURL -> Endpoint -> RequestMethod -> JsonCodec a -> m (Either String (Tuple Token a))
apiRequestWithToken url endpoint method codec = do
  res <- liftAff <<< request $ defaultRequest url Nothing endpoint method
  pure $ case res of
    Left err -> Left $ printError err
    Right resp -> do
      result <- lmap printJsonDecodeError $ Codec.decode codec resp.body
      case head resp.headers of
        Nothing -> Left "No headers present!"
        Just header -> Right $ Tuple (Token $ value header) result

-- The response contains a `Token` in the header, so `login` cannot just use
-- `apiRequest`.
login :: forall m. MonadAff m =>
  BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login url form = apiRequestWithToken url Login method profileCodec
  where
    method = Post <<< Just $ Codec.encode loginFieldsCodec form

register :: forall m. MonadAff m =>
  BaseURL -> RegisterFields -> m (Either String (Tuple Token Profile))
register url form = apiRequestWithToken url Register method profileCodec
  where
  method = Post $ Just $ Codec.encode registerFieldsCodec form

currentUser :: BaseURL -> Aff (Maybe Profile)
currentUser baseUrl = do
    mToken <- liftEffect readToken
    case mToken of
        Nothing -> pure Nothing
        Just token -> do
          res <- request $ defaultRequest baseUrl (Just token) User Get
          pure $ case res of 
                Left _ -> Nothing
                Right v -> hush $ do
                   u <- Codec.decode (CAR.object "User" { user: CA.json }) v.body
                   CA.decode profileCodec u.user

greet :: forall m. MonadAff m => BaseURL -> m (Either String String)
greet url = apiRequest url Greet Get CA.string

tokenKey :: String
tokenKey = "token"

-- The local storage of your HTML window contains a key-value map, where the
-- token is stored under the key `tokenKey`.
readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

writeToken :: Token -> Effect Unit
writeToken (Token str) = setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken = removeItem tokenKey =<< localStorage =<< window
