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

data CoachProposalResponse
  = AcceptProposal
  | DenyProposal
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity CoachProposalResponse

instance PersistField CoachProposalResponse where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql CoachProposalResponse where
  sqlType _ = SqlString

instance PathPiece CoachProposalResponse where
  fromPathPiece = readMaybe . T.unpack
  toPathPiece = T.pack . show

data JoinStatus
  = Cancelled
  | WillCome
  | WasPresent
  | WasAbsent
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity JoinStatus

instance PersistField JoinStatus where
  toPersistValue = toPersistValue . show
  fromPersistValue v = mapLeft T.pack . readEither =<< fromPersistValue v

instance PersistFieldSql JoinStatus where
  sqlType _ = SqlString

instance PathPiece JoinStatus where
  fromPathPiece = readMaybe . T.unpack
  toPathPiece = T.pack . show

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
