module Pact.Web.Server.Handler.HomeSpec (spec) where

import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec $
  describe "HomeR" $
    it "GETs a 200" $ do
      get HomeR
      statusIs 200
