{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Coach.Activities
  ( getAddActivityR,
    postAddActivityR,
    AddCoachWorkoutForm (..),
  )
where

import qualified Data.Text as T
import Data.Time.Calendar
import Data.Validity.Time ()
import Pact.Web.Server.Handler.Prelude

getAddActivityR :: WorkoutType -> Handler Html
getAddActivityR workoutType = defaultLayout $ do
  messages <- getMessages
  token <- genToken
  setTitleI ("Activity" :: Text)
  $(widgetFile "coach/activities")

data AddCoachWorkoutForm = AddCoachWorkoutForm
  { amountACWF :: Double,
    dayACWF :: Day,
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
    <*> ireq textareaField "notes"

postAddActivityR :: WorkoutType -> Handler Html
postAddActivityR workoutType =
  runInputPostResult addCoachWorkoutForm >>= \case
    FormSuccess form -> addCoachWorkout form workoutType
    FormMissing -> do
      addMessage "is-danger" "No form was filled in"
      redirect . CoachR $ AddActivityR workoutType
    FormFailure errors -> do
      forM_ errors $ addMessage "is-danger" . toHtml
      redirect . CoachR $ AddActivityR workoutType

addCoachWorkout :: AddCoachWorkoutForm -> WorkoutType -> Handler Html
addCoachWorkout AddCoachWorkoutForm {..} workoutType = do
  Coach {..} <- getCoach
  runDB $
    insert_
      CoachWorkout
        { coachWorkoutCoach = coachUuid,
          coachWorkoutType = workoutType,
          coachWorkoutDay = dayACWF,
          coachWorkoutAmount = amount,
          coachWorkoutNotes = notesACWF
        }
  addMessage "is-success" "Congratz, you did a workout!"
  redirect $ WorkoutR ActivitiesR
  where
    amount = WorkoutAmount . round $ amountACWF / stepSize workoutType

-- TODO: Think whether this should fail if `amountAWF` isn't almost exactly
-- equal to a positive integer multiple of the stepsize.
