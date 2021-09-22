{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.NewsfeedSpec (spec) where

import qualified Database.Persist.Class as P
import qualified Database.Persist.Types as P
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "NewsfeedR" $ do
  describe "AddUserWorkoutR" $ do
    wType <- liftIO $ generate genValid
    testRequiresLogin "NewsfeedR AddUserWorkoutR" . NewsfeedR $ AddUserWorkoutR wType

    it "POST succeeds" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form ->
        forAllValid $ \workoutType -> runYesodClientM yc $ do
          testRegisterUser testUser
          testCanReach . NewsfeedR $ AddUserWorkoutR workoutType
          submitUserWorkout form workoutType

    it "POST creates the correct user workout" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form ->
        forAllValid $ \workoutType -> runYesodClientM yc $ do
          testRegisterUser testUser
          testCanReach . NewsfeedR $ AddUserWorkoutR workoutType
          submitUserWorkout form workoutType
          userWorkouts <- testDB $ P.selectList [] []
          case userWorkouts of
            [P.Entity _ UserWorkout {..}] ->
              liftIO $ userWorkoutType `shouldBe` workoutType
            xs -> fail $ "Found " <> show (length xs) <> " user workouts instead of 1"

  describe "ConnectResponseR" $ do
    it "POST to respond to a non-existent request, results in `notFound`" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \coach ->
        forAllValid $ \response -> runYesodClientM yc $ do
          testRegisterUser testUser
          user <- getSingleUser
          testLogout
          testRegisterUser coach
          becomeCoach
          post . NewsfeedR $ ConnectCoachResponseR (userUuid user) response
          statusIs 404

    it "POST to respond to an unanswered request, updates the dB" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \testCoach ->
        forAllValid $ \response -> runYesodClientM yc $ do
          testRegisterUser testUser
          user <- getSingleUser
          testLogout

          testRegisterUser testCoach
          becomeCoach
          coach <- getSingleCoach
          testLogout

          testLoginUser testUser
          testSendConnectionProposal coach
          testLogout

          testLoginUser testCoach
          testRespondToProposal (userUuid user) response
          relations <- testDB $ selectListVals [] []
          case relations of
            [relation] -> liftIO $ customerCoachRelationResponse relation `shouldBe` Just response
            xs -> fail $ "Found " <> show (length xs) <> " customer coach relations instead of 1"

    it "POST to respond to an answered request, results in `notFound`" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \testCoach ->
        forAllValid $ \response -> forAllValid $ \response' -> runYesodClientM yc $ do
          testRegisterUser testUser
          user <- getSingleUser
          testLogout

          testRegisterUser testCoach
          becomeCoach
          coach <- getSingleCoach
          testLogout

          testLoginUser testUser
          testSendConnectionProposal coach
          testLogout

          testLoginUser testCoach
          testRespondToProposal (userUuid user) response

          post . NewsfeedR $ ConnectCoachResponseR (userUuid user) response'
          statusIs 404
