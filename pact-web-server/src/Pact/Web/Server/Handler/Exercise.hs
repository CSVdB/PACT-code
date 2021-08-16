{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Exercise where

import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Data.Validity
import Database.Persist.Types
import Pact.Web.Server.Handler.Import
import Text.Read

difficulties :: [Difficulty]
difficulties = [minBound .. maxBound]

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
    notesEF :: Maybe Textarea
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

addExerciseForm :: FormInput Handler AddExerciseForm
addExerciseForm =
  AddExerciseForm
    <$> ireq textField "exerciseName"
    <*> ireq difficultyField "difficulty"
    <*> ireq textareaField "formTips"
    <*> iopt textareaField "notes"
  where
    parseDifficulty = mapLeft T.pack . readEither
    toDifficulty :: Text -> Handler (Either FormMessage Difficulty)
    toDifficulty = pure . mapLeft MsgInvalidEntry . parseDifficulty . T.unpack
    difficultyField = checkMMap toDifficulty (T.pack . show) textField

postAddR :: Handler Html
postAddR = do
  res <-
    runInputPostResult $
      (,,)
        <$> addExerciseForm
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
  exUuid <- liftIO nextRandom
  imageUuid <- liftIO nextRandom
  videoUuid <- liftIO nextRandom
  imageContents <- fileSourceByteString ii
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
  addMessage "is-success" "Successfully submitted an exercise!"
  redirect . ExerciseR $ ViewR exUuid

getViewR :: ExerciseUUID -> Handler Html
getViewR uuid = do
  res <- runDB $ getBy $ UniqueExerciseUUID uuid
  case res of
    Nothing -> notFound
    Just (Entity _ Exercise {..}) -> defaultLayout $ do
      setTitleI exerciseName
      $(widgetFile "exercise/view")
