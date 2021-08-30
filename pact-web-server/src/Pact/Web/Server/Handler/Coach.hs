{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Coach
  ( getProfileR,
    postProfileR,
    ProfileForm (..),
  )
where

import qualified Data.Text as T
import Data.UUID.Typed (nextRandomUUID)
import Pact.Web.Server.Handler.Import

data Profile = Profile
  { profileName :: Username,
    profilePic :: Maybe ImageUUID,
    profileAboutMe :: Textarea
  }
  deriving (Show, Eq, Ord, Generic)

data IsCoach = IsCoach | NoCoachYet
  deriving (Show, Eq, Ord, Generic)

getUser :: Handler User
getUser = do
  mAuth <- maybeAuth
  case mAuth of
    Nothing -> notFound -- This isn't really possible, it's already stopped by Yesod authorization
    Just (Entity _ user) -> pure user

getProfile :: Handler (Profile, IsCoach)
getProfile = do
  User {..} <- getUser
  mCoach <- runDB $ getBy $ UniqueCoach userUuid
  pure $ case mCoach of
    Nothing ->
      let defaultProfile =
            Profile
              { profileName = userName,
                profilePic = Nothing,
                profileAboutMe = ""
              }
       in (defaultProfile, NoCoachYet)
    Just (Entity _ Coach {..}) ->
      let profile =
            Profile
              { profileName = userName,
                profilePic = coachPic,
                profileAboutMe = coachAboutMe
              }
       in (profile, IsCoach)

getProfileR :: Handler Html
getProfileR = do
  (Profile {..}, isCoach) <- getProfile
  token <- genToken
  defaultLayout $ do
    case isCoach of
      IsCoach -> setTitleI ("Profile" :: Text)
      NoCoachYet -> setTitleI ("Become coach" :: Text)
    $(widgetFile "coach/profile")

newtype ProfileForm = ProfileForm
  { aboutMePF :: Textarea
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity ProfileForm where
  validate form@ProfileForm {..} =
    mconcat
      [ genericValidate form,
        declare "\"About me\" isn't empty" . not . T.null $ unTextarea aboutMePF
      ]

profileForm :: FormInput Handler ProfileForm
profileForm = ProfileForm <$> ireq textareaField "about-me"

postProfileR :: Handler Html
postProfileR = do
  res <- runInputPostResult $ (,) <$> profileForm <*> iopt fileField "image"
  case res of
    FormSuccess (form, mImageInfo) -> upsertProfile form mImageInfo
    FormMissing -> do
      addMessage "is-danger" "No form was filled in"
      getProfileR
    FormFailure errors -> do
      forM_ errors $ addMessage "is-danger" . toHtml
      getProfileR

upsertProfile :: ProfileForm -> Maybe FileInfo -> Handler Html
upsertProfile ProfileForm {..} mImageInfo = do
  User {..} <- getUser
  (oldProfile, isCoach) <- getProfile
  mImageUuid <- forM mImageInfo $ \fileInfo -> do
    imageUuid <- liftIO nextRandomUUID
    -- Don't collect the contents within runDB, throwing exceptions could cause
    -- serious problems
    imageContents <- fileSourceByteString fileInfo
    let image =
          Image
            { imageUuid = imageUuid,
              imageContents = imageContents,
              imageTyp = fileContentType fileInfo
            }
    void . runDB $ upsert image [] -- Just insert if it doesn't exist yet
    pure imageUuid
  let imageUuid = case mImageUuid of
        Nothing -> profilePic oldProfile
        Just uuid -> Just uuid
  let coach =
        Coach
          { coachUuid = userUuid,
            coachPic = imageUuid,
            coachAboutMe = aboutMePF
          }
  void . runDB $ upsert coach [CoachPic =. mImageUuid, CoachAboutMe =. aboutMePF]
  case isCoach of
    IsCoach -> addMessage "is-success" "Successfully updated your profile!"
    NoCoachYet -> addMessage "is-success" "You're a coach now!"
  redirect $ CoachR ProfileR
