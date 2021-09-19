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
    postJoinActivityR,
    postCancelActivityR,
  )
where

import Data.List (sortOn, (\\))
import Data.Maybe (fromMaybe)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Validity.Time ()
import Pact.Web.Server.Handler.Prelude

getActivitiesR :: Handler Html
getActivitiesR = do
  user <- getUser
  mCoach <- getCoachM
  today <- liftIO $ utctDay <$> getCurrentTime
  coachWorkoutInfos <-
    sortCWIs today . fromMaybe []
      <$> forM mCoach (runDB . getCoachWorkoutInfos . snd)
  myCoachesWorkoutInfosTot <- sortCWIs today <$> runDB (getMyCoachesWorkoutInfos user)
  myPlannedWorkouts <-
    sortOn (dayCWI . snd) . filter (filterCondition today . snd)
      <$> runDB (userPlannedWorkouts user)
  let myCoachesWorkoutInfos = myCoachesWorkoutInfosTot \\ (snd <$> myPlannedWorkouts)
  defaultLayout $ do
    messages <- getMessages
    token <- genToken
    setTitleI ("Activities" :: Text)
    $(widgetFile "workout/activities")
  where
    sortCWIs today = sortOn dayCWI . filter (filterCondition today)
    filterCondition today CoachWorkoutInfo {..} = today <= dayCWI

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

postJoinActivityR :: CoachWorkoutUUID -> Handler Html
postJoinActivityR workoutUUID = do
  User {..} <- getUser
  runDB $
    insert_
      WorkoutJoin
        { workoutJoinCustomer = userUuid,
          workoutJoinWorkout = workoutUUID,
          workoutJoinCancelled = NotCancelled
        }
  addMessage "is-success" "You joined a workout, this will be great fun!"
  redirect $ WorkoutR ActivitiesR

postCancelActivityR :: CoachWorkoutUUID -> Handler Html
postCancelActivityR workoutUUID = do
  User {..} <- getUser
  res <-
    runDB $ do
      workout <- fmap entityVal <$> getBy (UniqueCoachWorkout workoutUUID)
      forM workout $ \CoachWorkout {..} ->
        getBy (UniqueJoin userUuid coachWorkoutUuid) >>= \case
          Nothing -> pure Nothing
          Just (Entity key WorkoutJoin {..}) -> case workoutJoinCancelled of
            Cancelled -> notFound
            NotCancelled -> Just <$> update key [WorkoutJoinCancelled =. Cancelled]
  case res of
    Nothing -> notFound
    Just _ -> do
      addMessage "is-success" "You joined a workout, this will be great fun!"
      redirect $ WorkoutR ActivitiesR
