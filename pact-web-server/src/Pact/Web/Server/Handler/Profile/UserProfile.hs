{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.Profile.UserProfile where

import qualified Data.Text as T
import Pact.Web.Server.Handler.Prelude

newtype UserProfileForm
  = UserProfileForm
      { aboutMeUPF :: Textarea
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity UserProfileForm where
  validate form@UserProfileForm {..} =
    mconcat
      [ genericValidate form,
        declare "\"About me\" isn't empty" . not . T.null $
          unTextarea aboutMeUPF
      ]

userProfileForm :: FormInput Handler UserProfileForm
userProfileForm = UserProfileForm <$> ireq textareaField "about-me"

postUpdateUserProfileR :: Handler Html
postUpdateUserProfileR = do
  res <- runInputPostResult $ (,) <$> userProfileForm <*> iopt fileField "image"
  case res of
    FormSuccess (form, mImageInfo) -> updateUserProfile form mImageInfo
    FormMissing -> do
      addMessage "is-danger" "No form was filled in"
      redirect $ ProfileR UpdateUserProfileR
    FormFailure errors -> do
      forM_ errors $ addMessage "is-danger" . toHtml
      redirect $ ProfileR UpdateUserProfileR

-- Note: This might be (<|>).
overrideMaybe :: Maybe a -> Maybe a -> Maybe a
overrideMaybe (Just a) _ = Just a
overrideMaybe Nothing b = b

updateUserProfile :: UserProfileForm -> Maybe FileInfo -> Handler Html
updateUserProfile UserProfileForm {..} mImageInfo = do
  User {..} <- getUser
  userKey <-
    runDB (getBy $ UniqueUser userUuid) >>= \case
      Nothing -> notFound
      Just (Entity key _) -> pure key
  mImageUuid <- forM mImageInfo addImage
  let imageUuid = overrideMaybe mImageUuid userPic
      aboutMe =
        if aboutMeUPF /= ""
          then aboutMeUPF
          else userAboutMe
  void . runDB $ update userKey [UserPic =. imageUuid, UserAboutMe =. aboutMe]
  addMessage "is-success" "Successfully updated your profile!"
  redirect $ ProfileR ProfilePageR
