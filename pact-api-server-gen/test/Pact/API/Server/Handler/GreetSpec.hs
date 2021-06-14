module Pact.API.Server.Handler.GreetSpec
  ( spec,
  )
where

import Pact.API
import Pact.API.Server.TestUtils
import Pact.Client
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  serverSpec $
    describe "GetGreet" $
      it "does not crash" $
        \cenv ->
          withAnyNewUser cenv $ \token -> do
            shouldBeValid =<< testClientOrErr cenv (getGreet pactClient token)
