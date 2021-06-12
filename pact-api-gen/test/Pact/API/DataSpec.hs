{-# LANGUAGE TypeApplications #-}

module Pact.API.DataSpec
  ( spec,
  )
where

import Pact.API.Data
import Pact.API.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @RegistrationForm
  genValidSpec @LoginForm
