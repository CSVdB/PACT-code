module Test.PACT.MySpec where

import Prelude
import PACT.AppM (runAppM)
import PACT.Store (LogLevel(..))
import PACT.API.Request (BaseURL(..))
import PACT.Data.Email (EmailAddress(..))
import PACT.Data.User (mkUsername, RegisterFields)
import PACT.Capability.User (registerUser)
import Data.Maybe (fromJust, Maybe(..), isJust)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

spec :: Spec Unit
spec =
  describe "register" $
    it "returns a profile" $ do
      mProfile <- runAppM initialStore $ registerUser fields
      mProfile `shouldSatisfy` isJust
  where
    fields :: RegisterFields
    fields =
      { username: unsafePartial $ fromJust $ mkUsername "Nick"
      , email: EmailAddress "vandenbroeck@cs-vdb.com"
      , password: "password"
      }
    initialStore =
      { logLevel: Dev
      , baseURL: BaseURL "http://localhost:8000"
      , currentUser: Nothing
      }
