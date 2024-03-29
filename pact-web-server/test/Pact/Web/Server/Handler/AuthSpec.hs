{-# LANGUAGE OverloadedStrings #-}

module Pact.Web.Server.Handler.AuthSpec (spec) where

import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec $ do
  describe "RegisterR" $ do
    it "GETs a 200" $ do
      get $ AuthR registerR
      statusIs 200

    it "can POST" $ \yc ->
      forAllValid $ \testUser -> runYesodClientM yc $ testRegisterUser testUser

    it "POST fails on mismatching passwords" $ \yc -> do
      forAllValid $ \username -> forAll genValidPassword $ \password ->
        forAll genValidPassword $ \password2 ->
          runYesodClientM yc $
            if password == password2
              then pure ()
              else testRegisterFail username password password2

    it "POST fails if username is already in use" $ \yc ->
      forAllValid $ \testUser ->
        forAll genValidPassword $ \password -> runYesodClientM yc $ do
          testRegisterUser testUser
          testLogout
          testRegisterFail (testUsername testUser) password password

  describe "LogoutR" $ do
    it "doesn't crash" $ \yc ->
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testLogout

    it "doesn't crash" $ \yc ->
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testLogout
        testCannotReach $ ExerciseR AddR

  describe "LoginR" $ do
    it "GETs a 200" $ do
      get $ AuthR LoginR
      statusIs 200

    it "can POST" $ \yc ->
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testLogout
        testLoginUser testUser

    it "POST fails with wrong password" $ \yc ->
      forAllValid $ \testUser -> forAll genValidPassword $ \password ->
        runYesodClientM yc $ do
          testRegisterUser testUser
          testLogout
          testLoginFailed (testUsername testUser) password

    it "POST fails with wrong username" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \username ->
        runYesodClientM yc $ do
          testRegisterUser testUser
          testLogout
          testLoginFailed username $ testUserPassword testUser

    it "Home page GETs a 200 as a user" $ \yc -> do
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testCanReach HomeR
