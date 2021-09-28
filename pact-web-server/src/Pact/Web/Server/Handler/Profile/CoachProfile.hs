{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Profile.CoachProfile where

import qualified Data.Text as T
import Pact.Web.Server.Handler.Prelude

newtype CoachProfileForm = CoachProfileForm
  { expertiseCPF :: Textarea
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity CoachProfileForm where
  validate form@CoachProfileForm {..} =
    mconcat
      [ genericValidate form,
        declare "Expertise is non-empty" . not . T.null $
          unTextarea expertiseCPF
      ]

coachProfileForm :: FormInput Handler CoachProfileForm
coachProfileForm = CoachProfileForm <$> ireq textareaField "expertise"

postUpdateCoachProfileR :: Handler Html
postUpdateCoachProfileR =
  runInputPostResult coachProfileForm >>= \case
    FormSuccess form -> updateCoachProfile form
    FormMissing -> do
      addMessage "is-danger" "No form was filled in"
      redirect $ ProfileR UpdateCoachProfileR
    FormFailure errors -> do
      forM_ errors $ addMessage "is-danger" . toHtml
      redirect $ ProfileR UpdateCoachProfileR

updateCoachProfile :: CoachProfileForm -> Handler Html
updateCoachProfile CoachProfileForm {..} = do
  User {..} <- getUser
  runDB (getBy $ UniqueCoachUser userUuid) >>= \case
    Nothing -> notFound
    Just (Entity key _) ->
      if expertiseCPF == ""
        then pure ()
        else void . runDB $ update key [CoachExpertise =. expertiseCPF]
  addMessage "is-success" "Profile updated"
  redirect $ ProfileR ProfilePageR
