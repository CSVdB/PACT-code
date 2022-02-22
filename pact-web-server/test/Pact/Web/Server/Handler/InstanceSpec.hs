{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Web.Server.Handler.InstanceSpec (spec) where

import Pact.Web.Server.Handler.TestImport
import Yesod

spec :: Spec
spec = do
  describe "CoachProposalResponse" $ do
    showReadSpec @CoachProposalResponse
    it "toPathPiece DenyProposal" $ toPathPiece DenyProposal `shouldBe` "DenyProposal"
    it "fromPathPiece DenyProposal" $ fromPathPiece "DenyProposal" `shouldBe` Just DenyProposal
    pathPieceSpec @CoachProposalResponse
    persistSpec @CoachProposalResponse
  describe "Difficulty" $ do
    showReadSpec @Difficulty
    jsonSpec @Difficulty
    persistSpec @Difficulty
  describe "Muscle" $ do
    showReadSpec @Muscle
    persistSpec @Muscle
  describe "Username" $ do
    persistSpec @Username
  describe "UserUUID" $ do
    jsonSpec @UserUUID
    persistSpec @UserUUID
    pathPieceSpec @UserUUID
