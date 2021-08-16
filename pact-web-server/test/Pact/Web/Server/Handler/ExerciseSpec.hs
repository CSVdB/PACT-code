{-# LANGUAGE OverloadedStrings #-}

module Pact.Web.Server.Handler.ExerciseSpec (spec) where

import qualified Database.Persist.Class as P
import qualified Database.Persist.Types as P
import Pact.DB
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
  describe "ViewR" $ do
    it "GETs 200 if logged in" $ \yc -> do
      forAllValid $ \testUser -> forAllValid $ \form -> runYesodClientM yc $ do
        testRegisterUser testUser
        testSubmitExercise form
        exercises <- testDB $ P.selectFirst [] []
        case exercises of
          Nothing -> fail "No exercise present in the dB after submitting one"
          Just (P.Entity _ exercise) -> testCanReach . ExerciseR . ViewR $ exerciseUuid exercise
