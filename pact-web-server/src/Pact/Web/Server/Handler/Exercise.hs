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

import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Pact.Web.Server.Handler.Exercise.Add
import Pact.Web.Server.Handler.Import

getViewR :: ExerciseUUID -> Handler Html
getViewR uuid = do
  res <- runDB $ collectExercise uuid
  case res of
    Nothing -> notFound
    Just CompleteExercise {..} -> defaultLayout $ do
      let Exercise {..} = exerciseCE
      setTitleI exerciseName
      $(widgetFile "exercise/view")

getViewAllR :: Handler Html
getViewAllR = do
  exercises <- fmap (fmap entityVal) . runDB $ selectList [] []
  completeExercises <- fmap catMaybes $ runDB $ forM exercises $ collectExercise . exerciseUuid
  defaultLayout $ do
    setTitleI ("Exercises" :: Text)
    $(widgetFile "exercise/view-all")
