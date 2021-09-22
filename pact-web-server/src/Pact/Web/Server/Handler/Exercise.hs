{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Exercise
  ( getAddR,
    postAddR,
    getViewR,
    AddExerciseForm (..),
    getViewAllR,
  )
where

import qualified Data.Text as T
import Pact.Web.Server.Handler.Exercise.Add
import Pact.Web.Server.Handler.Prelude

getViewR :: ExerciseUUID -> Handler Html
getViewR uuid =
  runDB (collectExercise uuid) >>= \case
    Nothing -> notFound
    Just CompleteExercise {..} -> defaultLayout $ do
      let Exercise {..} = exerciseCE
      setTitleI exerciseName
      $(widgetFile "exercise/view")

getViewAllR :: Handler Html
getViewAllR = do
  exercises <- runDB $ selectListVals [] []
  completeExercises <- fmap catMaybes $ runDB $ forM exercises $ collectExercise . exerciseUuid
  defaultLayout $ do
    setTitleI ("Exercises" :: Text)
    $(widgetFile "exercise/view-all")
