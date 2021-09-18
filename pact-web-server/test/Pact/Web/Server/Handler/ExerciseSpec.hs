{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.ExerciseSpec (spec) where

import qualified Database.Persist.Class as P
import qualified Database.Persist.Types as P
import Pact.DB
import Pact.Web.Server.Handler.Exercise
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "Exercise" $ do
  describe "AddR" $ do
    testRequiresCoach (ExerciseR AddR) "ExerciseR AddR"

    it "can POST when logged in" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form ->
        forAllValid $ \profileForm -> runYesodClientM yc $ do
          testRegisterUser testUser
          testProfileUpsert profileForm
          testSubmitExercise form

    it "POST an exercise adds the right number of muscle filters" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form ->
        forAllValid $ \profileForm -> runYesodClientM yc $ do
          testRegisterUser testUser
          testProfileUpsert profileForm
          testSubmitExercise form
          muscleFilters <- testDB $ P.selectList [] [] -- Collect all muscle filters
          let musclesDB =
                muscleFilterMuscle . P.entityVal <$> muscleFilters
          liftIO $ musclesDB `shouldBeSort` musclesEF form

    it "`collectExercise` returns what was POST AddR-ed" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form ->
        forAllValid $ \profileForm -> runYesodClientM yc $ do
          testRegisterUser testUser
          testProfileUpsert profileForm
          testSubmitExercise form
          exerciseM <- testDB $ P.selectFirst [] [] -- Find the exercise
          case exerciseM of
            Nothing -> fail "No exercise was added"
            Just (P.Entity _ ex) -> do
              res <- testDB . collectExercise $ exerciseUuid ex
              case res of
                Nothing -> fail "No exercise was found with this UUID"
                Just CompleteExercise {..} -> liftIO $ do
                  let Exercise {..} = exerciseCE
                  musclesCE `shouldBeSort` musclesEF form
                  exerciseName `shouldBe` nameEF form
                  (unMaterial <$> materialsCE) `shouldBeSort` (exerciseMaterialName <$> materialsEF form)
                  altNamesCE `shouldBeSort` altNamesEF form

  describe "ViewR" $ do
    it "GETs 200 if logged in" $ \yc -> do
      forAllValid $ \testUser -> forAllValid $ \testCoach ->
        forAllValid $ \form -> forAllValid $ \exerciseForm -> runYesodClientM yc $ do
          testRegisterUser testCoach
          testProfileUpsert form
          testSubmitExercise exerciseForm
          testLogout
          testRegisterUser testUser
          exercises <- testDB $ P.selectFirst [] []
          case exercises of
            Nothing -> fail "No exercise present in the dB after submitting one"
            Just (P.Entity _ exercise) -> testCanReach . ExerciseR . ViewR $ exerciseUuid exercise

    it "GETs redirect to login page if not logged in" $ \yc -> do
      forAllValid $ \testCoach -> forAllValid $ \form ->
        forAllValid $ \exerciseForm -> runYesodClientM yc $ do
          testRegisterUser testCoach
          testProfileUpsert form
          testSubmitExercise exerciseForm
          exercises <- testDB $ P.selectFirst [] []
          case exercises of
            Nothing -> fail "No exercise present in the dB after submitting one"
            Just (P.Entity _ exercise) -> do
              testLogout
              testCannotReach . ExerciseR . ViewR $ exerciseUuid exercise

  describe "ViewAllR" $ do
    testRequiresLogin (ExerciseR ViewAllR) "ExerciseR ViewAllR"
