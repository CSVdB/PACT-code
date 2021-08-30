{-# LANGUAGE OverloadedStrings #-}

module Pact.Web.Server.Handler.CoachSpec (spec) where

import qualified Database.Persist.Class as P
import qualified Database.Persist.Types as P
import Pact.Web.Server.Handler.Coach
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "Coach" $ do
  describe "Profile" $ do
    it "GETs 200 if logged in" $ \yc -> do
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testCanReach $ CoachR ProfileR

    it "GETs 303 redirect to Login if not logged in" $ \yc ->
      runYesodClientM yc $ testCannotReach $ CoachR ProfileR

    it "GET without logged in, then follow the redirect, ends up at AddR" $
      \yc -> forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testLogout
        testCannotReach $ CoachR ProfileR
        _ <- followRedirect
        loginRequest (testUsername testUser) $ testUserPassword testUser
        statusIs 303
        locationShouldBe $ CoachR ProfileR
        _ <- followRedirect
        statusIs 200

    it "can POST when logged in" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form -> runYesodClientM yc $ do
        testRegisterUser testUser
        testProfileUpsert form

    it "POST creates the correct Coach concept" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form -> runYesodClientM yc $ do
        testRegisterUser testUser
        testProfileUpsert form
        coaches <- fmap (fmap P.entityVal) $ testDB $ P.selectList [] []
        liftIO $ length coaches `shouldBe` 1
        liftIO $ aboutMePF form `shouldBe` coachAboutMe (head coaches)

    it "POSTing a second time overrides the first Coach" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form ->
        forAllValid $ \form2 -> runYesodClientM yc $ do
          testRegisterUser testUser
          testProfileUpsert form
          testProfileUpsert form2
          coaches <- fmap (fmap P.entityVal) $ testDB $ P.selectList [] []
          liftIO $ length coaches `shouldBe` 1 -- Still only one coach exists
          -- Second form is the end result
          liftIO $ aboutMePF form2 `shouldBe` coachAboutMe (head coaches)

  describe "ListR" $ do
    it "GETs 200 if logged in" $ \yc -> do
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testCanReach $ CoachR ListR
