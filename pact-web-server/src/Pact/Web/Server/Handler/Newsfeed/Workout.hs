{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Newsfeed.Workout where

import Data.Time.Calendar
import Pact.Web.Server.Handler.Prelude

getAddUserWorkoutR :: WorkoutType -> Handler Html
getAddUserWorkoutR wType = defaultLayout $ do
  messages <- getMessages
  token <- genToken
  today <- getCurrentDay
  setTitleI ("Workout" :: Text)
  $(widgetFile "newsfeed/addUserWorkout")

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

postAddUserWorkoutR :: WorkoutType -> Handler Html
postAddUserWorkoutR workoutType =
  runInputPostResult addWorkoutForm >>= \case
    FormSuccess form -> addWorkout form workoutType
    FormMissing -> do
      addMessage "is-danger" "No form was filled in"
      redirect . NewsfeedR $ AddUserWorkoutR workoutType
    FormFailure errors -> do
      forM_ errors $ addMessage "is-danger" . toHtml
      redirect . NewsfeedR $ AddUserWorkoutR workoutType

-- TODO: Think whether this should fail if `amountAWF` isn't almost exactly
-- equal to a positive integer multiple of the stepsize.
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
