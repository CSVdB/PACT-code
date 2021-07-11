{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.API.Data.Gen where

import Control.Monad
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Pact.DB
import Pact.Data
import Path
import Servant.Client.Core
import Test.QuickCheck

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

instance GenUnchecked Difficulty

instance GenValid Difficulty

instance GenUnchecked MuscleGroup

instance GenValid MuscleGroup

instance GenValid FormTip where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance Validity Scheme

instance GenUnchecked Scheme

instance GenValid Scheme

-- Generate a string of between `m` and `n` alphabetic characters.
genAlpha :: Int -> Int -> Gen String
genAlpha m n = do
  numberOfChars <- chooseInt (m, n)
  replicateM numberOfChars $ choose ('a', 'z')

instance GenValid BaseUrl where
  genValid = do
    scheme <- genValid
    host <- genHost
    port <- chooseInt (0, 65535)
    path <- toFilePath <$> genValid @(Path Rel File)
    pure $ BaseUrl scheme host port path
    where
      genHost = do
        domainName <- genAlpha 1 25
        extension <- genAlpha 1 7
        pure $ domainName <> "." <> extension
  shrinkValid = shrinkValidStructurally

instance GenValid Exercise where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
