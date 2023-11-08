{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.Handler.ProfileSpec
  ( spec,
  )
where

import Pact.Web.Server.Handler.Activities.Workout
import Pact.Web.Server.Handler.Newsfeed.Workout
import Pact.Web.Server.Handler.Profile
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "ProfileR" $ do
  testRequiresLogin "ProfileR ProfilePageR" $ ProfileR ProfilePageR
  testRequiresLogin "ProfileR ListCoachesR" $ ProfileR ListCoachesR
  testRequiresLogin "ProfileR ListCurrentCoachesR" $ ProfileR ListCurrentCoachesR
  testRequiresLogin "ProfileR ListCurrentCustomersR" $ ProfileR ListCurrentCustomersR
  testRequiresLogin "ProfileR ListCurrentFriendsR" $ ProfileR ListCurrentFriendsR
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
              "Found "
                <> show (length xs)
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
  describe "UpdateUserProfileR" $ do
    testRequiresLogin "ProfileR UpdateUserProfileR" $ ProfileR UpdateUserProfileR
    it "POST suceeds" $ \yc ->
      forAllValid $ \user -> forAllValid $ \profile ->
        runYesodClientM yc $ do
          testRegisterUser user
          testUpdateUserProfile profile
    it "POST creates the correct coach profile" $ \yc ->
      forAllValid $ \user -> forAllValid $ \profile ->
        runYesodClientM yc $ do
          testRegisterUser user
          testUpdateUserProfile profile
          User {..} <- getSingleUser
          liftIO $ userAboutMe `shouldBe` aboutMeUPF profile
    it "POST a second time overrides the first Coach" $ \yc ->
      forAllValid $ \user -> forAllValid $ \profile ->
        forAllValid $ \profile' -> runYesodClientM yc $ do
          testRegisterUser user
          testUpdateUserProfile profile
          testUpdateUserProfile profile'
          User {..} <- getSingleUser
          liftIO $ userAboutMe `shouldBe` aboutMeUPF profile'
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
  testRequiresLogin "ProfileR ListFriendsR" $ ProfileR ListFriendsR
  describe "ConnectFriendR" $ do
    it "POST suceeds" $ \yc ->
      forAllValid $ \user1 -> forAllValid $ \user2 -> runYesodClientM yc $ do
        testRegisterUser user1
        user <- getSingleUser
        testLogout
        testRegisterUser user2
        testSendFriendRequest user
    it "POST creates the correct friend request" $ \yc -> do
      forAllValid $ \user1 -> forAllValid $ \user2 -> runYesodClientM yc $ do
        testRegisterUser user1
        user <- getSingleUser
        testLogout
        testRegisterUser user2
        testSendFriendRequest user
        friendProposals <- testDB $ selectListVals [] []
        case friendProposals of
          [FriendRelation {..}] ->
            liftIO $ friendRelationReceiver `shouldBe` userUuid user
          xs -> fail $ "Found " <> show (length xs) <> " friend proposal instead of 1"
    it "POSTing twice to the same user gives notFound" $ \yc -> do
      forAllValid $ \user1 -> forAllValid $ \user2 -> runYesodClientM yc $ do
        testRegisterUser user1
        user <- getSingleUser
        testLogout
        testRegisterUser user2
        testSendFriendRequest user
        post . ProfileR . ConnectFriendR $ userUuid user
        statusIs 404
  describe "countCoins" $ do
    it "Calculates correctly" $ \yc ->
      forAllValid $ \user -> forAllValid $ \coach -> forAllValid $ \form ->
        forAllValid $ \workoutType -> forAllValid $ \userForm -> runYesodClientM yc $ do
          let userWorkoutTime = dayAWF userForm
              coachWorkoutTime = dayACWF form
              timeInterval =
                if userWorkoutTime <= coachWorkoutTime
                  then (userWorkoutTime, coachWorkoutTime)
                  else (coachWorkoutTime, userWorkoutTime)
          testRegisterUser user
          userId <- userUuid <$> getSingleUser
          testLogout
          testRegisterCoach coach
          submitCoachWorkout form workoutType
          coachWorkoutId <- coachWorkoutUuid <$> getSingleCoachWorkout
          testLogout
          testLoginUser user
          coins1 <- testDB $ countCoins userId timeInterval
          liftIO $ coins1 `shouldBe` Coins 0
          submitUserWorkout userForm workoutType
          coins2 <- testDB $ countCoins userId timeInterval
          liftIO $ coins2 `shouldBe` Coins 3
          joinWorkout coachWorkoutId
          coins3 <- testDB $ countCoins userId timeInterval
          liftIO $ coins3 `shouldBe` Coins 4
          -- Confirm you went to the workout
          post $ ActivitiesR $ UpdateCoachWorkoutJoinR coachWorkoutId WasPresent
          coins4 <- testDB $ countCoins userId timeInterval
          liftIO $ coins4 `shouldBe` Coins 8
