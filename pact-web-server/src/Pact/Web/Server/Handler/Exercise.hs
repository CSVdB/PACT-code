{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Exercise where

import Data.Functor
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Data.Validity
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
  res <- runInputPostResult addExerciseForm
  token <- genToken
  case res of
    FormSuccess form -> addExercise form
    FormMissing -> do
      addMessage "is-danger" "No form was filled in"
      messages <- getMessages
      defaultLayout $(widgetFile "exercise/add")
    FormFailure errors -> do
      forM_ errors $ addMessage "is-danger" . toHtml
      messages <- getMessages
      defaultLayout $(widgetFile "exercise/add")

addExercise :: AddExerciseForm -> Handler Html
addExercise AddExerciseForm {..} = do
  uuid <- liftIO nextRandom
  void . runDB $
    insert
      Exercise
        { exerciseUuid = uuid,
          exerciseName = nameEF,
          exerciseDifficulty = difficultyEF,
          exerciseFormTips = unTextarea formTipsEF,
          exerciseNotes = maybe "" unTextarea notesEF
        }
  addMessage "is-success" "Successfully submitted an exercise!"
  -- TODO: Redirect to the newly created exercise instead
  -- redirect $ ExerciseR uuid
  redirect HomeR
