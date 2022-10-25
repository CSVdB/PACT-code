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
    dayAWF :: Day,
    descriptionAWF :: Textarea
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity AddUserWorkoutForm where
  validate form@AddUserWorkoutForm {..} =
    mconcat
      [ genericValidate form,
        declare "amountAWF is positive" $ amountAWF > 0,
        declare "amountAWF is finite" . not $ isInfinite amountAWF,
        declare "amountAWF is not NaN" . not $ isNaN amountAWF
      ]

addWorkoutForm :: FormInput Handler AddUserWorkoutForm
addWorkoutForm =
  AddUserWorkoutForm
    <$> ireq doubleField "amount"
    <*> ireq dayField "day"
    <*> (fromMaybe "" <$> iopt textareaField "description")

postAddUserWorkoutR :: WorkoutType -> Handler Html
postAddUserWorkoutR workoutType =
  runInputPostResult ((,) <$> addWorkoutForm <*> iopt fileField "image")
    >>= \case
      FormSuccess (form, mImageInfo) -> addWorkout form mImageInfo workoutType
      FormMissing -> do
        addMessage "is-danger" "No form was filled in"
        redirect . NewsfeedR $ AddUserWorkoutR workoutType
      FormFailure errors -> do
        forM_ errors $ addMessage "is-danger" . toHtml
        redirect . NewsfeedR $ AddUserWorkoutR workoutType

-- TODO: Think whether this should fail if `amountAWF` isn't almost exactly
-- equal to a positive integer multiple of the stepsize.
addWorkout :: AddUserWorkoutForm -> Maybe FileInfo -> WorkoutType -> Handler Html
addWorkout AddUserWorkoutForm {..} mImageInfo workoutType = do
  User {..} <- getUser
  mImageUuid <- forM mImageInfo addImage
  runDB $
    insert_
      UserWorkout
        { userWorkoutUser = userUuid,
          userWorkoutType = workoutType,
          userWorkoutDay = dayAWF,
          userWorkoutAmount = amount,
          userWorkoutDescription = descriptionAWF,
          userWorkoutImage = mImageUuid
        }
  addMessage "is-success" "Congratz, you did a workout!"
  redirect HomeR
  where
    amount = WorkoutAmount . round $ amountAWF / stepSize workoutType
