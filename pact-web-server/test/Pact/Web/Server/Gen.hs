{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.UUID ()
import qualified Data.Text as T
import Pact.DB
import Pact.Data
import Pact.Web.Server.Handler
import Test.QuickCheck

instance GenValid Password where
  genValid = mkPassword <$> genValid
  shrinkValid _ = [] -- No point.

instance GenValid (PasswordHash Bcrypt) where
  -- This is technically more than necessary, but we can't do any better because hashPassword runs in IO.
  genValid = PasswordHash <$> genValid
  shrinkValid _ = [] -- No point.

instance GenValid User where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

data TestUser = TestUser
  { testUsername :: Username,
    testUserPassword :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity TestUser

genValidPassword :: Gen Text
genValidPassword = genValid `suchThat` (not . T.null)

instance GenValid TestUser where
  genValid = TestUser <$> genValid <*> genValidPassword
  shrinkValid _ = []

instance GenValid Difficulty where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Textarea where
  genValid = Textarea <$> genValid
  shrinkValid (Textarea t) = Textarea <$> shrinkValid t

instance GenValid AddExerciseForm where
  genValid = genValidStructurally `suchThat` isValid
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid Image where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Video where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
