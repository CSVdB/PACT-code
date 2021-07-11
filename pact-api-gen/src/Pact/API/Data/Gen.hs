{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.API.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Pact.Data

instance GenValid EmailAddress where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid RegistrationForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Profile where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
