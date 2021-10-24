{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Activities.Workout where

import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Pact.Web.Server.Handler.Prelude

getAddCoachWorkoutR :: WorkoutType -> Handler Html
getAddCoachWorkoutR workoutType = defaultLayout $ do
  messages <- getMessages
  token <- genToken
  today <- getCurrentDay
  setTitleI ("Activity" :: Text)
  $(widgetFile "activities/workout")

data AddCoachWorkoutForm = AddCoachWorkoutForm
  { amountACWF :: Double,
    dayACWF :: Day,
    timeOfDayACWF :: TimeOfDay,
    addressACWF :: Textarea,
    notesACWF :: Textarea
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity AddCoachWorkoutForm where
  validate form@AddCoachWorkoutForm {..} =
    mconcat
      [ genericValidate form,
        declare "notesACWF isn't empty" . not . T.null $ unTextarea notesACWF
      ]

addCoachWorkoutForm :: FormInput Handler AddCoachWorkoutForm
addCoachWorkoutForm =
  AddCoachWorkoutForm
    <$> ireq doubleField "amount"
    <*> ireq dayField "day"
    <*> ireq timeField "timeOfDay"
    <*> ireq textareaField "address"
    <*> ireq textareaField "notes"

postAddCoachWorkoutR :: WorkoutType -> Handler Html
postAddCoachWorkoutR workoutType =
  runInputPostResult addCoachWorkoutForm >>= \case
    FormSuccess form -> addCoachWorkout form workoutType
    FormMissing -> do
      addMessage "is-danger" "No form was filled in"
      redirect . ActivitiesR $ AddCoachWorkoutR workoutType
    FormFailure errors -> do
      forM_ errors $ addMessage "is-danger" . toHtml
      redirect . ActivitiesR $ AddCoachWorkoutR workoutType

addCoachWorkout :: AddCoachWorkoutForm -> WorkoutType -> Handler Html
addCoachWorkout AddCoachWorkoutForm {..} workoutType = do
  (_, Coach {..}) <- getCoach
  uuid <- nextRandomUUID
  runDB $
    insert_
      CoachWorkout
        { coachWorkoutUuid = uuid,
          coachWorkoutCoach = coachUuid,
          coachWorkoutAmount = amount,
          coachWorkoutType = workoutType,
          coachWorkoutDay = dayACWF,
          coachWorkoutTimeOfDay = timeOfDayACWF,
          coachWorkoutAddress = addressACWF,
          coachWorkoutNotes = notesACWF
        }
  addMessage "is-success" "Great, you're organizing a workout!"
  redirect $ ActivitiesR ActivitiesPageR
  where
    amount = WorkoutAmount . round $ amountACWF / stepSize workoutType

-- TODO: Think whether this should fail if `amountAWF` isn't almost exactly
-- equal to a positive integer multiple of the stepsize.
