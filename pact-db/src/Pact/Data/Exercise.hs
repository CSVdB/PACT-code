{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.Exercise where

import Data.Aeson
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Data.Validity
import Data.Validity.UUID ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read (readEither)

type ExerciseUUID = UUID

instance PersistField UUID where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql UUID where
  sqlType _ = SqlString

data Difficulty = Easy | Medium | Hard deriving (Show, Eq, Ord, Generic, Read)

instance Validity Difficulty -- Any value is valid

instance FromJSON Difficulty

instance ToJSON Difficulty

instance PersistField Difficulty where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql Difficulty where
  sqlType _ = SqlString

type FormTips = Text
