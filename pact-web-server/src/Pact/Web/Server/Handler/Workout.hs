{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Workout
  ( getActivitiesR,
    getUserR,
    postUserR,
    AddUserWorkoutForm (..),
  )
where

import Data.Maybe (fromMaybe, isJust)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Validity.Time ()
import Pact.Web.Server.Handler.Prelude

getActivitiesR :: Handler Html
getActivitiesR = do
  mCoach <- getCoachM
  coachWorkouts <- fromMaybe [] <$> forM mCoach (runDB . getCoachWorkouts . snd)
  defaultLayout $ do
    messages <- getMessages
    token <- genToken
    setTitleI ("Activities" :: Text)
    $(widgetFile "workout/activities")

getUserR :: WorkoutType -> Handler Html
getUserR wType = defaultLayout $ do
  messages <- getMessages
  token <- genToken
  today <- liftIO $ utctDay <$> getCurrentTime
  setTitleI ("Workout" :: Text)
  $(widgetFile "workout/user")

data AddUserWorkoutForm = AddUserWorkoutForm
  { amountAWF :: Double,
    dayAWF :: Day
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity AddUserWorkoutForm

addWorkoutForm :: FormInput Handler AddUserWorkoutForm
addWorkoutForm =
  AddUserWorkoutForm
    <$> ireq doubleField "amount"
    <*> ireq dayField "day"

postUserR :: WorkoutType -> Handler Html
postUserR workoutType =
  runInputPostResult addWorkoutForm >>= \case
    FormSuccess form -> addWorkout form workoutType
    FormMissing -> do
      addMessage "is-danger" "No form was filled in"
      redirect . WorkoutR $ UserR workoutType
    FormFailure errors -> do
      forM_ errors $ addMessage "is-danger" . toHtml
      redirect . WorkoutR $ UserR workoutType

addWorkout :: AddUserWorkoutForm -> WorkoutType -> Handler Html
addWorkout AddUserWorkoutForm {..} workoutType = do
  User {..} <- getUser
  runDB $
    insert_
      UserWorkout
        { userWorkoutUser = userUuid,
          userWorkoutType = workoutType,
          userWorkoutDay = dayAWF,
          userWorkoutAmount = amount
        }
  addMessage "is-success" "Congratz, you did a workout!"
  redirect HomeR
  where
    amount = WorkoutAmount . round $ amountAWF / stepSize workoutType

-- TODO: Think whether this should fail if `amountAWF` isn't almost exactly
-- equal to a positive integer multiple of the stepsize.
