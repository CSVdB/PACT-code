module Pact.API.Server.Handler.AuthSpec
  ( spec,
  )
where

import Data.Functor (void)
import Network.HTTP.Types as HTTP
import Pact.API
import Pact.API.Data.Gen ()
import Pact.API.Server.TestUtils
import Pact.Client
import Pact.Data
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec =
  serverSpec $ do
    describe "PostRegister" $
      it "does not crash" $
        \cenv ->
          forAllValid $ \rf ->
            void . testClientOrErr cenv $ postRegister pactClient rf
    describe "PostRegister" $ do
      it "fails before registration" $ \cenv ->
        forAllValid $ \lf -> do
          errOrRes <- testClient cenv $ postLogin pactClient lf
          case errOrRes of
            Left err ->
              case err of
                FailureResponse _ resp
                  | responseStatusCode resp == HTTP.unauthorized401 -> pure ()
                _ ->
                  failure $
                    "Should have errored with code 401, got this instead: "
                      <> show err
            _ -> failure "Should have errored"
      it
        "shows no difference between a login failure for a user that exists and a user that doesn't exist"
        $ \cenv ->
          forAllValid $ \un1 ->
            forAll (genValid `suchThat` (/= un1)) $ \un2 ->
              forAllValid $ \pw1 ->
                forAllValid $ \em1 ->
                  -- Sign up user 1 but not user 2
                  forAll (genValid `suchThat` (/= pw1)) $ \pw2 -> do
                    void . testClientOrErr cenv . postRegister pactClient $
                      RegistrationForm
                        { registrationFormUsername = un1,
                          registrationFormPassword = pw1,
                          registrationFormEmail = em1
                        }
                    errOrRes1 <-
                      testClient cenv $
                        postLogin pactClient $
                          LoginForm
                            { loginFormUsername = un1,
                              loginFormPassword = pw2
                            }
                    errOrRes2 <-
                      testClient cenv $
                        postLogin pactClient $
                          LoginForm
                            { loginFormUsername = un2,
                              loginFormPassword = pw2
                            }
                    () <$ errOrRes1 `shouldBe` () <$ errOrRes2
      it "succeeds after registration" $ \cenv ->
        forAllValid $ \rf -> void . testClientOrErr cenv $ do
          _ <- postRegister pactClient rf
          postLogin pactClient $
            registrationFormToLoginForm rf
