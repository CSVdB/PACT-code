{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.Proposal where

import Data.Either.Combinators (mapLeft)
import qualified Data.Text as T
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read
import Yesod

data CoachCoachProposalResponse
  = AcceptProposal
  | DenyProposal
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity CoachCoachProposalResponse

instance PersistField CoachCoachProposalResponse where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql CoachCoachProposalResponse where
  sqlType _ = SqlString

instance PathPiece CoachCoachProposalResponse where
  fromPathPiece = readMaybe . T.unpack
  toPathPiece = T.pack . show

data Cancelled
  = Cancelled
  | NotCancelled
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity Cancelled

instance PersistField Cancelled where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql Cancelled where
  sqlType _ = SqlString

data FriendRequestResponse
  = AcceptFriend
  | DenyFriend
  | CancelledFriendRequest
  | CancelledFriendRelation
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity FriendRequestResponse

instance PersistField FriendRequestResponse where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PathPiece FriendRequestResponse where
  fromPathPiece = readMaybe . T.unpack
  toPathPiece = T.pack . show

instance PersistFieldSql FriendRequestResponse where
  sqlType _ = SqlString

prettyPrintFriendRequestResponse :: Maybe FriendRequestResponse -> String
prettyPrintFriendRequestResponse = \case
  Just AcceptFriend -> "Accepted"
  Just DenyFriend -> "Denied"
  Just CancelledFriendRequest -> "Cancelled request"
  Just CancelledFriendRelation -> "Cancelled friend"
  Nothing -> "Proposed"

data FriendType = Proposer | Receiver
  deriving (Show, Eq, Ord, Generic)
