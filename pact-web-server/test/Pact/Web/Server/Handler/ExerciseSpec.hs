{-# LANGUAGE OverloadedStrings #-}

module Pact.Web.Server.Handler.ExerciseSpec (spec) where

import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "Exercise" $ do
  describe "AddR" $ do
    it "GETs a 200 if logged in" $ \yc -> do
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testCanReach $ ExerciseR AddR
    it "GET a 303 redirect to Login if not logged in" $ \yc ->
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
        testCanReach $ ExerciseR AddR
        testFile <- readTestFile "test-resources/exercise/image/pushup.jpg"
        addExerciseRequest form testFile
        statusIs 303
        locationShouldBe HomeR -- TODO: Fix this once individual exercises can be visualized
        _ <- followRedirect
        statusIs 200
