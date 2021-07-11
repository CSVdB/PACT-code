{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.Exercise where

import Data.Aeson
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Servant.Client.Core (BaseUrl, parseBaseUrl, showBaseUrl)
import Text.Read (readEither)

data Difficulty = Easy | Medium | Hard deriving (Show, Eq, Ord, Generic, Read)

instance Validity Difficulty -- Any value is valid

instance FromJSON Difficulty

instance ToJSON Difficulty

instance PersistField Difficulty where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql Difficulty where
  sqlType _ = SqlString

data WorkoutType
  = Push
  | Pull
  | Core
  | Legs
  | FullBody
  | Cardio
  deriving (Show, Eq, Ord, Generic)

data MuscleGroup
  = Shoulders
  | Abs
  | Biceps
  | Triceps
  | Forearms
  | Quads
  | Calves
  | Hamstrings
  | UpperChest
  | LowerChest
  | UpperBack
  | LowerBack
  deriving (Show, Eq, Ord, Generic, Read)

instance Validity MuscleGroup -- Any value is valid

instance FromJSON MuscleGroup

instance ToJSON MuscleGroup

instance PersistField MuscleGroup where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql MuscleGroup where
  sqlType _ = SqlString

-- TODO: Serve all images and videos on our own website (e.g.
-- `static.pactcommunity.be`), generate a hash for each, and replace `BaseUrl`
-- by `hash + ".png"` or "hash + ".mp4"`.
type SourceURI = BaseUrl

instance Validity BaseUrl where
  validate baseUrl = case parseBaseUrl $ showBaseUrl baseUrl of
    Left err -> invalid $ show err
    Right _ -> valid

instance PersistField BaseUrl where
  toPersistValue = toPersistValue . showBaseUrl
  fromPersistValue (PersistText t) = case parseBaseUrl $ T.unpack t of
    Left err -> Left $ T.pack $ show err
    Right baseUrl -> Right baseUrl
  fromPersistValue _ = Left "No text"

instance PersistFieldSql BaseUrl where
  sqlType _ = SqlString

newtype FormTip = FormTip {unFormTip :: Text} deriving (Show, Eq, Ord, Generic)

mkFormTip :: Text -> Maybe FormTip
mkFormTip t
  | T.any (== '\n') t = Nothing
  | otherwise = Just $ FormTip t

instance Validity FormTip where
  validate (FormTip t) =
    mconcat
      [ check (not $ T.any (== '\n') t) "FormTip doesn't contain newlines",
        delve "FormTip contains a valid Text" t
      ]

instance FromJSON FormTip where
  parseJSON = withText "FormTip" $ \t ->
    if T.any (== '\n') t
      then fail "FormTips cannot contain newlines"
      else pure $ FormTip t

instance ToJSON FormTip

instance PersistField FormTip where
  toPersistValue (FormTip t) = toPersistValue t
  fromPersistValue (PersistText t) = case mkFormTip t of
    Nothing -> Left "Contains newlines"
    Just formTip -> Right formTip
  fromPersistValue _ = Left "Not text"

instance PersistFieldSql FormTip where
  sqlType _ = SqlString
