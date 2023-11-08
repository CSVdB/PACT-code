{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Exercise.Add where

import qualified Data.Map as Map
import qualified Data.Text as T
import Pact.Web.Server.Handler.Prelude

getAddR :: Handler Html
getAddR =
  runDB collectAllMaterials >>= \materials -> defaultLayout $ do
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
    materialsEF :: [Text],
    altNamesEF :: [AlternativeName]
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity AddExerciseForm where
  validate form@AddExerciseForm {..} =
    mconcat
      [ genericValidate form,
        declare "nameEF isn't empty" . not $ T.null nameEF,
        declare "formTipsEF isn't empty" . not . T.null $ unTextarea formTipsEF,
        declare "All muscles are different" . not $ hasDuplicates musclesEF,
        declare "All exercise materials are different" . not $ hasDuplicates materialsEF,
        declare "All alternative names are different" . not $ hasDuplicates altNamesEF
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
    <*> altNamesInput
  where
    materialsInput = fromMaybe [] <$> iopt materialsField "materials"
    matToOption ExerciseMaterial {..} =
      Option
        { optionDisplay = exerciseMaterialName,
          optionInternalValue = exerciseMaterialName,
          optionExternalValue = exerciseMaterialName
        }
    materialsField =
      checkboxesField . pure $
        mkOptionList $
          matToOption <$> allMaterials
    altNamesInput = altNames . fromMaybe "" <$> iopt textField "alternativeNames"

postAddR :: Handler Html
postAddR = do
  allMaterials <- runDB collectAllMaterials
  res <-
    runInputPostResult $
      (,,)
        <$> addExerciseForm allMaterials
        <*> ireq fileField "image"
        <*> ireq fileField "video"
  case res of
    FormSuccess (form, imageInfo, videoInfo) -> addExercise form imageInfo videoInfo allMaterials
    FormMissing -> do
      addMessage "is-danger" "No form was filled in"
      redirect $ ExerciseR AddR
    FormFailure errors -> do
      forM_ errors $ addMessage "is-danger" . toHtml
      redirect $ ExerciseR AddR

addExercise ::
  AddExerciseForm ->
  FileInfo ->
  FileInfo ->
  [ExerciseMaterial] ->
  Handler Html
addExercise AddExerciseForm {..} ii vi materials = do
  runDB (getBy $ UniqueExerciseName nameEF) >>= \mExercise ->
    when (isJust mExercise) $ do
      addMessage "is-danger" "An exercise with this name already exists!"
      redirect $ ExerciseR AddR

  exUuid <- liftIO nextRandomUUID
  imageUuid <- liftIO nextRandomUUID
  videoUuid <- liftIO nextRandomUUID
  -- Don't collect the contents within runDB, throwing exceptions could cause
  -- serious problems
  imageContents <- fileSourceByteString ii
  videoContents <- fileSourceByteString vi
  materialUuids <- forM materialsEF $ \name -> maybe notFound pure $ materialMap Map.!? name
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
    forM_ materialUuids $ \uuid ->
      insert_
        MaterialFilter
          { materialFilterExercise = exUuid,
            materialFilterMaterial = uuid
          }
    forM_ altNamesEF $ \name ->
      insert_
        ExerciseAlternativeName
          { exerciseAlternativeNameUuid = exUuid,
            exerciseAlternativeNameName = unAlternative name
          }
  addMessage "is-success" "Successfully submitted an exercise!"
  redirect . ExerciseR $ ViewR exUuid
  where
    materialMap =
      Map.fromList $
        materials
          <&> \ExerciseMaterial {..} ->
            (exerciseMaterialName, exerciseMaterialUuid)
