{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.UUID where

import qualified Data.ByteString.Lazy as LB
import qualified Data.UUID as UUID
import Data.UUID.Typed
import Data.Validity.UUID ()
import Database.Persist
import Database.Persist.Sql
import Yesod

data E -- Phantom type representing exercises

data I -- Phantom type representing images

data V -- Phantom type representing videos

data M -- Phantom type representing exercise materials

type ExerciseUUID = UUID E

type ImageUUID = UUID I

type VideoUUID = UUID V

type ExerciseMaterialUUID = UUID M

instance PersistField (UUID a) where
  toPersistValue (UUID uuid) = PersistByteString $ LB.toStrict $ UUID.toByteString uuid
  fromPersistValue pv = do
    bs <- fromPersistValue pv
    case UUID.fromByteString $ LB.fromStrict bs of
      Nothing -> Left "Invalid Bytestring to convert to UUID"
      Just uuid -> Right $ UUID uuid

instance PersistFieldSql (UUID a) where
  sqlType _ = SqlBlob -- UUIDs are bytestrings, so this is more efficient than SqlString.

instance PathPiece (UUID a) where
  fromPathPiece = parseUUIDText
  toPathPiece = uuidText
