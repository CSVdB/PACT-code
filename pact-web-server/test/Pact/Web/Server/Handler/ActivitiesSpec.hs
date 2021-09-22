{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.ActivitiesSpec (spec) where

import Pact.Web.Server.Handler.Activities
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "ActivitiesR" $ do
  describe "ActivitiesR ActivitiesPageR" $
    testRequiresLogin "ActivitiesR ActivitiesPageR" $ ActivitiesR ActivitiesPageR

  describe "ActivitiesR $ AddCoachWorkoutR workoutType" $ do
    wType <- liftIO $ generate genValid
    testRequiresCoach "ActivitiesR $ AddCoachWorkoutR workoutType" . ActivitiesR $ AddCoachWorkoutR wType

    it "can POST when logged in as coach" $ \yc ->
      forAllValid $ \testCoach -> forAllValid $ \form ->
        forAllValid $ \workoutType -> runYesodClientM yc $ do
          testRegisterUser testCoach
          becomeCoach
          submitCoachWorkout form workoutType

    it "POST adds the right coach workout to the dB" $ \yc ->
      forAllValid $ \testCoach -> forAllValid $ \form ->
        forAllValid $ \workoutType -> runYesodClientM yc $ do
          testRegisterUser testCoach
          becomeCoach
          submitCoachWorkout form workoutType
          CoachWorkout {..} <- getSingleCoachWorkout
          liftIO $ coachWorkoutType `shouldBe` workoutType
          liftIO $
            coachWorkoutAmount
              `shouldBe` WorkoutAmount (round $ amountACWF form / stepSize workoutType)

  describe "JoinCoachWorkoutR" $ do
    it "can POST when logged in" $ \yc ->
      forAllValid $ \user -> forAllValid $ \form ->
        forAllValid $ \coach -> forAllValid $ \workoutType ->
          runYesodClientM yc $ do
            testRegisterUser coach
            becomeCoach
            submitCoachWorkout form workoutType
            CoachWorkout {..} <- getSingleCoachWorkout
            testLogout
            testRegisterUser user
            joinWorkout coachWorkoutUuid

    it "POST adds the right workout to the dB" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \acwf ->
        forAllValid $ \testCoach -> forAllValid $ \workoutType ->
          runYesodClientM yc $ do
            testRegisterUser testCoach
            becomeCoach
            submitCoachWorkout acwf workoutType
            testLogout
            testRegisterUser testUser
            CoachWorkout {..} <- getSingleCoachWorkout
            joinWorkout coachWorkoutUuid
            WorkoutJoin {..} <- getSingleWorkoutJoin
            liftIO $ workoutJoinCancelled `shouldBe` NotCancelled

    it "POST to join the same workout twice, results in `notFound`" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \acwf ->
        forAllValid $ \testCoach -> forAllValid $ \workoutType ->
          runYesodClientM yc $ do
            testRegisterUser testCoach
            becomeCoach
            submitCoachWorkout acwf workoutType
            CoachWorkout {..} <- getSingleCoachWorkout

            testLogout
            testRegisterUser testUser
            joinWorkout coachWorkoutUuid
            post . ActivitiesR $ JoinCoachWorkoutR coachWorkoutUuid
            statusIs 404

  describe "CancelCoachWorkoutJoinR" $ do
    it "can POST when logged in" $ \yc ->
      forAllValid $ \user -> forAllValid $ \form ->
        forAllValid $ \coach -> forAllValid $ \workoutType ->
          runYesodClientM yc $ do
            testRegisterUser coach
            becomeCoach
            submitCoachWorkout form workoutType
            CoachWorkout {..} <- getSingleCoachWorkout

            testLogout
            testRegisterUser user
            joinWorkout coachWorkoutUuid
            cancelWorkout coachWorkoutUuid

    it "POST updates a WorkoutJoin appropriately in the dB" $ \yc ->
      forAllValid $ \user -> forAllValid $ \form ->
        forAllValid $ \coach -> forAllValid $ \workoutType ->
          runYesodClientM yc $ do
            testRegisterUser coach
            becomeCoach
            submitCoachWorkout form workoutType
            CoachWorkout {..} <- getSingleCoachWorkout

            testLogout
            testRegisterUser user
            joinWorkout coachWorkoutUuid
            cancelWorkout coachWorkoutUuid
            WorkoutJoin {..} <- getSingleWorkoutJoin
            liftIO $ workoutJoinCancelled `shouldBe` Cancelled

    it "POST to cancel a non-existent workout, results in `notFound`" $ \yc ->
      forAllValid $ \user -> forAllValid $ \form ->
        forAllValid $ \coach -> forAllValid $ \workoutType ->
          runYesodClientM yc $ do
            testRegisterUser coach
            becomeCoach
            submitCoachWorkout form workoutType
            CoachWorkout {..} <- getSingleCoachWorkout

            testLogout
            testRegisterUser user
            -- Didn't join the workout.
            post . ActivitiesR $ CancelCoachWorkoutJoinR coachWorkoutUuid
            statusIs 404

    it "POST to cancel the same workout twice, results in `notFound`" $ \yc ->
      forAllValid $ \user -> forAllValid $ \form ->
        forAllValid $ \coach -> forAllValid $ \workoutType ->
          runYesodClientM yc $ do
            testRegisterUser coach
            becomeCoach
            submitCoachWorkout form workoutType
            CoachWorkout {..} <- getSingleCoachWorkout

            testLogout
            testRegisterUser user
            joinWorkout coachWorkoutUuid
            cancelWorkout coachWorkoutUuid
            post . ActivitiesR $ CancelCoachWorkoutJoinR coachWorkoutUuid
            statusIs 404
