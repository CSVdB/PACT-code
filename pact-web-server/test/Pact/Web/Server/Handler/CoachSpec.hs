{-# LANGUAGE OverloadedStrings #-}

module Pact.Web.Server.Handler.CoachSpec (spec) where

import qualified Database.Persist.Class as P
import qualified Database.Persist.Types as P
import Pact.Web.Server.Handler.Coach
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "Coach" $ do
  describe "Profile" $ do
    testRequiresLogin (CoachR ProfileR) "CoachR ProfileR"

    it "can POST when logged in" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form -> runYesodClientM yc $ do
        testRegisterUser testUser
        testProfileUpsert form

    it "POST creates the correct Coach concept" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form -> runYesodClientM yc $ do
        testRegisterUser testUser
        testProfileUpsert form
        coaches <- fmap (fmap P.entityVal) $ testDB $ P.selectList [] []
        liftIO $ length coaches `shouldBe` 1
        liftIO $ aboutMePF form `shouldBe` coachAboutMe (head coaches)

    it "POSTing a second time overrides the first Coach" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form ->
        forAllValid $ \form2 -> runYesodClientM yc $ do
          testRegisterUser testUser
          testProfileUpsert form
          testProfileUpsert form2
          coaches <- fmap (fmap P.entityVal) $ testDB $ P.selectList [] []
          liftIO $ length coaches `shouldBe` 1 -- Still only one coach exists
          -- Second form is the end result
          liftIO $ aboutMePF form2 `shouldBe` coachAboutMe (head coaches)

  describe "ListR" $ do
    it "GETs 200 if logged in" $ \yc -> do
      forAllValid $ \testUser -> runYesodClientM yc $ do
        testRegisterUser testUser
        testCanReach $ CoachR ListR

  describe "ConnectR" $ do
    it "POST creates a connection request" $ \yc -> do
      forAllValid $ \user -> forAllValid $ \form ->
        forAllValid $ \user2 -> runYesodClientM yc $ do
          testRegisterUser user
          testProfileUpsert form
          coach <- fmap (P.entityVal . head) $ testDB $ P.selectList [] []
          testLogout
          testRegisterUser user2
          testSendConnectionProposal coach
          customerCoachRelations <- fmap (fmap P.entityVal) $ testDB $ P.selectList [] []
          liftIO $ do
            length (customerCoachRelations :: [CustomerCoachRelation])
              `shouldBe` 1
            customerCoachRelationCoach (head customerCoachRelations)
              `shouldBe` coachUuid coach

    it "Trying to connect to the same coach twice gives notFound" $ \yc -> do
      forAllValid $ \testUser -> forAllValid $ \testCoach ->
        forAllValid $ \form -> runYesodClientM yc $ do
          testRegisterUser testCoach
          testProfileUpsert form
          coach <- fmap (P.entityVal . head) $ testDB $ P.selectList [] []
          testLogout
          testRegisterUser testUser
          testSendConnectionProposal coach
          get $ CoachR ListR
          post . CoachR . ConnectR $ coachUuid coach
          statusIs 404

  describe "ConnectResponseR" $ do
    it "POST to respond to a non-existent request, results in `notFound`" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \testCoach ->
        forAllValid $ \form -> forAllValid $ \response -> runYesodClientM yc $ do
          testRegisterUser testUser
          user <- getSingleUser
          testLogout
          testRegisterUser testCoach
          testProfileUpsert form
          respondToProposalRequest (userUuid user) response
          statusIs 404

    it "POST to respond to an unanswered request, updates the dB" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \testCoach ->
        forAllValid $ \form -> forAllValid $ \response -> runYesodClientM yc $ do
          testRegisterUser testUser
          user <- getSingleUser
          testLogout

          testRegisterUser testCoach
          testProfileUpsert form
          coach <- getSingleCoach
          testLogout

          testLoginUser testUser
          testSendConnectionProposal coach
          testLogout

          testLoginUser testCoach
          testRespondToProposal (userUuid user) response
          relations <- fmap (fmap P.entityVal) . testDB $ P.selectList [] []
          case relations of
            [relation] -> liftIO $ customerCoachRelationResponse relation `shouldBe` Just response
            _ -> fail "Found an incorrect number of relations in the dB"

    it "POST to respond to an answered request, results in `notFound`" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \testCoach ->
        forAllValid $ \form -> forAllValid $ \response ->
          forAllValid $ \response' -> runYesodClientM yc $ do
            testRegisterUser testUser
            user <- getSingleUser
            testLogout

            testRegisterUser testCoach
            testProfileUpsert form
            coach <- getSingleCoach
            testLogout

            testLoginUser testUser
            testSendConnectionProposal coach
            testLogout

            testLoginUser testCoach
            testRespondToProposal (userUuid user) response

            respondToProposalRequest (userUuid user) response' -- Respond to the same request again
            statusIs 404

  describe "AddActivityR" $ do
    it "GETs 403 if logged in as non-coach user" $ \yc -> do
      forAllValid $ \testUser -> forAllValid $ \workoutType ->
        runYesodClientM yc $ do
          testRegisterUser testUser
          get . CoachR $ AddActivityR workoutType
          statusIs 403

    it "GETs 200 if logged in as coach" $ \yc -> do
      forAllValid $ \testUser -> forAllValid $ \form ->
        forAllValid $ \workoutType -> runYesodClientM yc $ do
          testRegisterUser testUser
          testProfileUpsert form
          testCanReach . CoachR $ AddActivityR workoutType

    it "GETs 303 redirect to Login if not logged in" $ \yc ->
      forAllValid $ runYesodClientM yc . testCannotReach . CoachR . AddActivityR

    it "GET without logged in redirects, eventually to (CoachR $ AddActivityR workoutType)" $
      \yc -> forAllValid $ \testUser -> forAllValid $ \workoutType ->
        forAllValid $ \form -> runYesodClientM yc $ do
          testRegisterUser testUser
          testProfileUpsert form
          testLogout
          testCannotReach . CoachR $ AddActivityR workoutType
          _ <- followRedirect
          loginRequest (testUsername testUser) $ testUserPassword testUser
          statusIs 303
          locationShouldBe . CoachR $ AddActivityR workoutType
          _ <- followRedirect
          statusIs 200
