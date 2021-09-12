{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.Coach where

import Data.Either.Combinators (mapLeft)
import qualified Data.Text as T
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read
import Yesod

data ProposalResponse
  = AcceptProposal
  | DenyProposal
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity ProposalResponse

instance PersistField ProposalResponse where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

-- instance PersistField ProposalResponse where
--   toPersistValue AcceptProposal = PersistBool True
--   toPersistValue DenyProposal = PersistBool False
--   fromPersistValue (PersistBool True) = Right AcceptProposal
--   fromPersistValue (PersistBool False) = Right DenyProposal
--   fromPersistValue _ = Left "Not a boolean"
--
instance PersistFieldSql ProposalResponse where
  sqlType _ = SqlString

instance PathPiece ProposalResponse where
  fromPathPiece = readMaybe . T.unpack
  toPathPiece = T.pack . show
