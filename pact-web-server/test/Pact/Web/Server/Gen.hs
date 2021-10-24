{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Gen where

import Data.Containers.ListUtils (nubOrd)
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import qualified Data.Text as T
import Pact.DB
import Pact.DB.Migrations (exerciseMaterialNames)
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

instance GenUnchecked Difficulty

instance GenValid Difficulty

instance GenUnchecked Muscle

instance GenValid Muscle

genExerciseMaterialName :: Gen Text
genExerciseMaterialName = elements exerciseMaterialNames

instance GenValid ExerciseMaterial where
  -- ExerciseMaterials are only valid once they're inserted into the dB
  genValid = ExerciseMaterial <$> genValid <*> genExerciseMaterialName
  shrinkValid = const []

instance GenValid Textarea where
  genValid = Textarea <$> genValid
  shrinkValid (Textarea t) = Textarea <$> shrinkValid t

instance GenValid AlternativeName where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

genAddExerciseFormInitial :: Gen AddExerciseForm
genAddExerciseFormInitial =
  AddExerciseForm
    <$> genValid <*> genValid <*> genValid <*> genValid <*> (nubOrd <$> genValid)
      <*> (nubOrd <$> listOf genExerciseMaterialName)
      <*> (nubOrd <$> genValid)

instance GenValid AddExerciseForm where
  genValid = genAddExerciseFormInitial `suchThat` isValid
  shrinkValid form =
    shrinkValidStructurally form <&> \newForm ->
      newForm
        { materialsEF = materialsEF form -- TODO: Keep the material UUIDs valid!
        }

instance GenValid UserProfileForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid CoachProfileForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Image where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Video where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked CoachProposalResponse

instance GenValid CoachProposalResponse

instance GenUnchecked FriendRequestResponse

instance GenValid FriendRequestResponse

instance GenValid AddUserWorkoutForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid AddCoachWorkoutForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked WorkoutType

instance GenValid WorkoutType

instance GenUnchecked JoinStatus

instance GenValid JoinStatus
