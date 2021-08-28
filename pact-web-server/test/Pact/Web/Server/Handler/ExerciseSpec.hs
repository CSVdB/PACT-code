{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.ExerciseSpec (spec) where

import Data.List (sort)
import qualified Database.Persist.Class as P
import qualified Database.Persist.Types as P
import Pact.DB
import Pact.Web.Server.Handler.Exercise
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "Exercise" $ do
  describe "AddR" $ do
    it "GETs 200 if logged in" $ \yc -> do
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testCanReach $ ExerciseR AddR
    it "GETs 303 redirect to Login if not logged in" $ \yc ->
      runYesodClientM yc $ testCannotReach $ ExerciseR AddR
    it "GET without logged in, then follow the redirect, ends up at AddR" $
      \yc -> forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testLogout
        testCannotReach $ ExerciseR AddR
        _ <- followRedirect
        loginRequest (testUsername testUser) $ testUserPassword testUser
        statusIs 303
        locationShouldBe $ ExerciseR AddR
        _ <- followRedirect
        statusIs 200
    it "can POST when logged in" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form -> runYesodClientM yc $ do
        testRegisterUser testUser
        testSubmitExercise form
    it "POST an exercise adds the right number of muscle filters" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form -> runYesodClientM yc $ do
        testRegisterUser testUser
        testSubmitExercise form
        muscleFilters <- testDB $ P.selectList [] [] -- Collect all muscle filters
        let musclesDB =
              muscleFilterMuscle . P.entityVal <$> muscleFilters
        liftIO $ sort musclesDB `shouldBe` sort (musclesEF form)
    it "`collectExercise` returns what was POST AddR-ed" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form -> runYesodClientM yc $ do
        testRegisterUser testUser
        testSubmitExercise form
        exerciseM <- testDB $ P.selectFirst [] [] -- Find the exercise
        case exerciseM of
          Nothing -> fail "No exercise was added"
          Just (P.Entity _ ex) -> do
            res <- testDB . collectExercise $ exerciseUuid ex
            case res of
              Nothing -> fail "No exercise was found with this UUID"
              Just (Exercise {..}, muscles, materials) -> liftIO $ do
                sort muscles `shouldBe` sort (musclesEF form)
                exerciseName `shouldBe` nameEF form
                sort (exerciseMaterialName <$> materials) `shouldBe` sort (exerciseMaterialName <$> materialsEF form)
  describe "ViewR" $ do
    it "GETs 200 if logged in" $ \yc -> do
      forAllValid $ \testUser -> forAllValid $ \form -> runYesodClientM yc $ do
        testRegisterUser testUser
        testSubmitExercise form
        exercises <- testDB $ P.selectFirst [] []
        case exercises of
          Nothing -> fail "No exercise present in the dB after submitting one"
          Just (P.Entity _ exercise) -> testCanReach . ExerciseR . ViewR $ exerciseUuid exercise
