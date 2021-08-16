{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.UUID where

import Data.Either.Combinators (mapLeft)
import qualified Data.Text as T
import Data.UUID
import Data.Validity.UUID ()
import Database.Persist
import Database.Persist.Sql
import Text.Read (readEither)
import Yesod

type ExerciseUUID = UUID

type ImageUUID = UUID

instance PersistField UUID where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql UUID where
  sqlType _ = SqlString

instance PathPiece UUID where
  fromPathPiece = fromText
  toPathPiece = toText
