{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.WorkoutSpec (spec) where

-- import Pact.DB
import qualified Database.Persist.Class as P
import qualified Database.Persist.Types as P
import Pact.Web.Server.Handler.TestImport
import Pact.Web.Server.Handler.Workout

spec :: Spec
spec = pactWebServerSpec . describe "Workout" $ do
  describe "UserR" $ do
    it "GETs 200 if logged in" $ \yc -> do
      forAllValid $ \testUser -> forAllValid $ \workoutType ->
        runYesodClientM yc $ do
          testRegisterUser testUser
          testCanReach . WorkoutR $ UserR workoutType

    it "GETs 303 redirect to Login if not logged in" $ \yc ->
      forAllValid $ runYesodClientM yc . testCannotReach . WorkoutR . UserR

    it "GET without logged in, then follow the redirect, ends up at UserR" $
      \yc -> forAllValid $ \testUser -> forAllValid $ \workoutType ->
        runYesodClientM yc $ do
          testRegisterUser testUser
          testLogout
          testCannotReach . WorkoutR $ UserR workoutType
          _ <- followRedirect
          loginRequest (testUsername testUser) $ testUserPassword testUser
          statusIs 303
          locationShouldBe . WorkoutR $ UserR workoutType
          _ <- followRedirect
          statusIs 200

    it "can POST when logged in" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form -> forAllValid $
        \workoutType -> runYesodClientM yc $ do
          testRegisterUser testUser
          submitUserWorkout form workoutType

    it "POST adds the right workout to the dB" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form -> forAllValid $
        \workoutType -> runYesodClientM yc $ do
          testRegisterUser testUser
          submitUserWorkout form workoutType
          workoutsDB <- testDB $ P.selectList [] [] -- Collect workouts
          case P.entityVal <$> workoutsDB of
            [UserWorkout {..}] -> liftIO $ do
              userWorkoutType `shouldBe` workoutType
              userWorkoutAmount
                `shouldBe` WorkoutAmount (round $ amountAWF form / stepSize workoutType)
            xs -> fail $ "Found " ++ show (length xs) ++ " workouts in the dB instead of 1"
