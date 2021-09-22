{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.ProfileSpec (spec) where

import Pact.Web.Server.Handler.Profile
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "ProfileR" $ do
  testRequiresLogin "ProfileR ProfilePageR" $ ProfileR ProfilePageR
  testRequiresLogin "ProfileR ListCoachesR" $ ProfileR ListCoachesR

  describe "ConnectCoachR" $ do
    it "POST creates the correct connection request" $ \yc -> do
      forAllValid $ \testCoach -> forAllValid $ \user -> runYesodClientM yc $ do
        testRegisterUser testCoach
        becomeCoach
        coach <- getSingleCoach
        testLogout
        testRegisterUser user
        testSendConnectionProposal coach
        customerCoachRelations <- testDB $ selectListVals [] []
        case customerCoachRelations of
          [CustomerCoachRelation {..}] ->
            liftIO $ customerCoachRelationCoach `shouldBe` coachUuid coach
          xs ->
            fail $
              "Found " <> show (length xs)
                <> " customer coach relations instead of 1"

    it "Trying to connect to the same coach twice gives notFound" $ \yc -> do
      forAllValid $ \testCoach -> forAllValid $ \user -> runYesodClientM yc $ do
        testRegisterUser testCoach
        becomeCoach
        coach <- getSingleCoach
        testLogout
        testRegisterUser user
        testSendConnectionProposal coach
        post . ProfileR . ConnectCoachR $ coachUuid coach
        statusIs 404

  describe "BecomeCoachR" $ do
    it "can POST when logged in" $ \yc ->
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        becomeCoach

    it "POST creates the correct Coach concept" $ \yc ->
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        becomeCoach
        Coach {..} <- getSingleCoach
        liftIO $ coachExpertise `shouldBe` ""

  describe "UpdateCoachProfileR" $ do
    testRequiresCoach "ProfileR UpdateCoachProfileR" $ ProfileR UpdateCoachProfileR

    it "POST suceeds" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \coachProfile ->
        runYesodClientM yc $ do
          testRegisterUser testUser
          becomeCoach
          testUpdateCoachProfile coachProfile

    it "POST creates the correct coach profile" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \coachProfile ->
        runYesodClientM yc $ do
          testRegisterUser testUser
          becomeCoach
          testUpdateCoachProfile coachProfile
          Coach {..} <- getSingleCoach
          liftIO $ coachExpertise `shouldBe` expertiseCPF coachProfile

    it "POST a second time overrides the first Coach" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \coachProfile ->
        forAllValid $ \coachProfile2 -> runYesodClientM yc $ do
          testRegisterUser testUser
          becomeCoach
          testUpdateCoachProfile coachProfile
          testUpdateCoachProfile coachProfile2
          Coach {..} <- getSingleCoach
          liftIO $ coachExpertise `shouldBe` expertiseCPF coachProfile2
