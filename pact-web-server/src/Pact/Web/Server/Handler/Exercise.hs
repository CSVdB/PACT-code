{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Exercise
  ( getAddR,
    postAddR,
    getViewR,
    AddExerciseForm (..),
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.UUID.Typed (nextRandomUUID)
import Data.Validity
import Pact.Web.Server.Handler.Import

allDifficulties :: [Difficulty]
allDifficulties = [minBound .. maxBound]

allMuscles :: [Muscle]
allMuscles = [minBound .. maxBound]

getAddR :: Handler Html
getAddR = defaultLayout $ do
  messages <- getMessages
  token <- genToken
  setTitle "Add exercise"
  $(widgetFile "exercise/add")

data AddExerciseForm = AddExerciseForm
  { nameEF :: Text,
    difficultyEF :: Difficulty,
    formTipsEF :: Textarea,
    notesEF :: Maybe Textarea,
    musclesEF :: [Muscle],
    materialsEF :: [ExerciseMaterial]
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Textarea where
  validate (Textarea t) = delve "Textarea" t

instance Validity AddExerciseForm where
  validate form@AddExerciseForm {..} =
    mconcat
      [ genericValidate form,
        declare "nameEF isn't empty" . not $ T.null nameEF,
        declare "formTipsEF isn't empty" . not . T.null $ unTextarea formTipsEF
      ]

addExerciseForm :: [ExerciseMaterial] -> FormInput Handler AddExerciseForm
addExerciseForm allMaterials =
  AddExerciseForm
    <$> ireq textField "exerciseName"
    <*> ireq (radioField optionsEnumShow) "difficulty"
    <*> ireq textareaField "formTips"
    <*> iopt textareaField "notes"
    <*> ireq (checkboxesField optionsEnumShow) "muscles"
    <*> materialsInput
  where
    materialsInput = fromMaybe [] <$> iopt materialsField "materials"
    matToOption mat@ExerciseMaterial {..} =
      Option
        { optionDisplay = exerciseMaterialName,
          optionInternalValue = mat,
          optionExternalValue = exerciseMaterialName
        }
    materialsField =
      checkboxesField . pure $
        mkOptionList $ matToOption <$> allMaterials

postAddR :: Handler Html
postAddR = do
  allMaterials <- runDB collectAllMaterials
  res <-
    runInputPostResult $
      (,,)
        <$> addExerciseForm allMaterials
        <*> ireq fileField "image"
        <*> ireq fileField "video"
  token <- genToken
  case res of
    FormSuccess (form, imageInfo, videoInfo) -> addExercise form imageInfo videoInfo
    FormMissing -> do
      addMessage "is-danger" "No form was filled in"
      messages <- getMessages
      defaultLayout $(widgetFile "exercise/add")
    FormFailure errors -> do
      forM_ errors $ addMessage "is-danger" . toHtml
      messages <- getMessages
      defaultLayout $(widgetFile "exercise/add")

addExercise :: AddExerciseForm -> FileInfo -> FileInfo -> Handler Html
addExercise AddExerciseForm {..} ii vi = do
  exUuid <- liftIO nextRandomUUID
  imageUuid <- liftIO nextRandomUUID
  videoUuid <- liftIO nextRandomUUID
  imageContents <- fileSourceByteString ii -- Don't do this within runDB, throwing exceptions could cause serious problems
  videoContents <- fileSourceByteString vi
  runDB $ do
    insert_
      Exercise
        { exerciseUuid = exUuid,
          exerciseImage = imageUuid,
          exerciseVideo = videoUuid,
          exerciseName = nameEF,
          exerciseDifficulty = difficultyEF,
          exerciseFormTips = unTextarea formTipsEF,
          exerciseNotes = maybe "" unTextarea notesEF
        }
    insert_
      Image
        { imageUuid = imageUuid,
          imageContents = imageContents,
          imageTyp = fileContentType ii
        }
    insert_
      Video
        { videoUuid = videoUuid,
          videoContents = videoContents,
          videoTyp = fileContentType vi
        }
    forM_ musclesEF $ \muscle ->
      insert_
        MuscleFilter
          { muscleFilterExercise = exUuid,
            muscleFilterMuscle = muscle
          }
    forM_ materialsEF $ \ExerciseMaterial {..} ->
      insert_
        MaterialFilter
          { materialFilterExercise = exUuid,
            materialFilterMaterial = exerciseMaterialUuid
          }
  addMessage "is-success" "Successfully submitted an exercise!"
  redirect . ExerciseR $ ViewR exUuid

getViewR :: ExerciseUUID -> Handler Html
getViewR uuid = do
  res <- runDB $ collectExercise uuid
  case res of
    Nothing -> notFound
    Just (Exercise {..}, muscles, _) -> defaultLayout $ do
      setTitleI exerciseName
      $(widgetFile "exercise/view")
