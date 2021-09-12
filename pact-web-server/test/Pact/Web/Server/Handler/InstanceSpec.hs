{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Web.Server.Handler.InstanceSpec (spec) where

import Pact.Web.Server.Handler.TestImport
import Yesod

spec :: Spec
spec = do
  describe "ProposalResponse" $ do
    showReadSpec @ProposalResponse
    it "toPathPiece DenyProposal" $ toPathPiece DenyProposal `shouldBe` "DenyProposal"
    it "fromPathPiece DenyProposal" $ fromPathPiece "DenyProposal" `shouldBe` Just DenyProposal
    pathPieceSpec @ProposalResponse
    persistSpec @ProposalResponse
  describe "Difficulty" $ do
    showReadSpec @Difficulty
    jsonSpec @Difficulty
    persistSpec @Difficulty
  describe "Muscle" $ do
    showReadSpec @Muscle
    persistSpec @Muscle
  describe "Username" $ do
    persistSpecOnValid @Username
  describe "UserUUID" $ do
    jsonSpecOnValid @UserUUID
    persistSpecOnValid @UserUUID
    pathPieceSpecOnValid @UserUUID
