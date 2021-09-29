{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.TestUtils where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (sort)
import qualified Data.Text as T
import qualified Database.Persist.Sql as DB
import Database.Persist.Sqlite (fkEnabled, mkSqliteConnectionInfo, runSqlPool, walEnabled, withSqlitePoolInfo)
import GHC.Generics
import Lens.Micro ((&), (.~))
import Network.HTTP.Client as HTTP
import Pact.DB.Migrations (allServerMigrations)
import Pact.Data
import Pact.Web.Server.Application ()
import Pact.Web.Server.Foundation
import Pact.Web.Server.Gen
import Pact.Web.Server.Handler hiding (get)
import Path.IO
import System.FilePath (takeExtension)
import System.Random (randomRIO)
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity as X
import Test.Syd.Wai (managerSpec)
import Test.Syd.Yesod

type PactWebServerSpec = YesodSpec App

pactWebServerSpec :: PactWebServerSpec -> Spec
pactWebServerSpec = modifyMaxSuccess (const 5) . managerSpec . yesodSpecWithSiteSetupFunc serverSetup

serverSetup :: HTTP.Manager -> SetupFunc App
serverSetup man = do
  tdir <- tempDirSetupFunc "pact"
  pool <- pactConnectionPoolSetupFunc
  sessionKeyFile <- resolveFile tdir "session-key.aes"
  pure
    App
      { appLogLevel = LevelWarn,
        appStatic = pactWebServerStatic,
        appHTTPManager = man,
        appConnectionPool = pool,
        appSessionKeyFile = sessionKeyFile,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }

pactConnectionPoolSetupFunc :: SetupFunc DB.ConnectionPool
pactConnectionPoolSetupFunc = SetupFunc $ \func ->
  runNoLoggingT . withSqlitePoolInfo info 1 $ \pool -> do
    runSqlPool allServerMigrations pool
    liftIO $ func pool
  where
    info = mkSqliteConnectionInfo ":memory:" & walEnabled .~ False & fkEnabled .~ False

testRegisterUser :: TestUser -> YesodExample App ()
testRegisterUser TestUser {..} = testRegister testUsername testUserPassword

testRegister :: Username -> Text -> YesodExample App ()
testRegister username password = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR registerR
    addToken
    addPostParam "username" $ usernameText username
    addPostParam "password" password
    addPostParam "confirmPassword" password
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200

testRegisterFail :: Username -> Text -> Text -> YesodExample App ()
testRegisterFail username password confirmPassword = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR registerR
    addToken
    addPostParam "username" $ usernameText username
    addPostParam "password" password
    addPostParam "confirmPassword" confirmPassword
  statusIs 303
  locationShouldBe $ AuthR registerR -- Failed to register
  _ <- followRedirect
  statusIs 200

testLogout :: YesodExample App ()
testLogout = do
  post $ AuthR LogoutR
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200

testLoginUser :: TestUser -> YesodExample App ()
testLoginUser TestUser {..} = testLogin testUsername testUserPassword

loginRequest :: Username -> Text -> YesodExample App ()
loginRequest username password = request $ do
  setMethod methodPost
  setUrl $ AuthR loginR
  addToken
  addPostParam "username" $ usernameText username
  addPostParam "password" password

testLogin :: Username -> Text -> YesodExample App ()
testLogin username password = do
  get $ AuthR LoginR
  statusIs 200
  loginRequest username password
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200

testLoginFailed :: Username -> Text -> YesodExample App ()
testLoginFailed username password = do
  get $ AuthR LoginR
  statusIs 200
  loginRequest username password
  statusIs 303
  locationShouldBe $ AuthR LoginR -- Failed to log in
  _ <- followRedirect
  statusIs 200

testCanReach :: Route App -> YesodExample App ()
testCanReach route = do
  get route
  statusIs 200

testCannotReach :: Route App -> YesodExample App ()
testCannotReach route = do
  get route
  statusIs 303
  locationShouldBe $ AuthR LoginR

data TestFile = TestFile
  { testFilePath :: !FilePath,
    testFileContents :: !ByteString,
    testFileType :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

readTestFile :: MonadIO m => FilePath -> m TestFile
readTestFile testFilePath = do
  testFileContents <- liftIO $ B.readFile testFilePath
  let testFileType = case takeExtension testFilePath of
        ".jpg" -> Just "image/jpeg"
        ".jpeg" -> Just "image/jpeg"
        ".png" -> Just "image/png"
        ".mp4" -> Just "video/mp4"
        _ -> Nothing
  pure TestFile {..}

addTestFileWith :: Text -> TestFile -> RequestBuilder App ()
addTestFileWith parameterName TestFile {..} =
  addFileWith parameterName testFilePath testFileContents testFileType

addExerciseRequest :: AddExerciseForm -> TestFile -> TestFile -> YesodExample App ()
addExerciseRequest AddExerciseForm {..} imageFile videoFile = request $ do
  setMethod methodPost
  setUrl $ ExerciseR AddR
  addToken
  addPostParam "exerciseName" nameEF
  addPostParam "difficulty" . T.pack $ show difficultyEF
  addPostParam "formTips" $ unTextarea formTipsEF
  addPostParam "notes" $ maybe "" unTextarea notesEF
  forM_ musclesEF $ \muscle -> addPostParam "muscles" $ T.pack $ show muscle
  addTestFileWith "image" imageFile
  addTestFileWith "video" videoFile
  forM_ materialsEF $ addPostParam "materials"
  case alternativeNamesText altNamesEF of
    "" -> pure ()
    txt -> addPostParam "alternativeNames" txt

submitExercise :: AddExerciseForm -> TestFile -> TestFile -> YesodExample App ()
submitExercise form imageFile videoFile = do
  testCanReach $ ExerciseR AddR
  addExerciseRequest form imageFile videoFile
  statusIs 303
  getLocation >>= \case
    Left err -> fail $ T.unpack err
    Right (ExerciseR (ViewR _)) -> do
      _ <- followRedirect
      statusIs 200
    Right _ -> fail "Redirect after submitting an exercise ends up in the wrong location"

testSubmitExercise :: AddExerciseForm -> YesodExample App ()
testSubmitExercise form = do
  imageFile <- readTestFile "test-resources/exercise/image/pushup.jpg"
  videoFile <- readTestFile "test-resources/exercise/video/explosive-pushup.mp4"
  submitExercise form imageFile videoFile

becomeCoach :: YesodExample App ()
becomeCoach = do
  testCanReach $ ProfileR ProfilePageR
  becomeCoachRequest
  statusIs 303
  locationShouldBe $ ProfileR ProfilePageR
  _ <- followRedirect
  statusIs 200
  where
    becomeCoachRequest = request $ do
      setMethod methodPost
      setUrl $ ProfileR BecomeCoachR
      addToken

updateUserProfileR :: UserProfileForm -> Maybe TestFile -> YesodExample App ()
updateUserProfileR UserProfileForm {..} mImageFile = request $ do
  setMethod methodPost
  setUrl $ ProfileR UpdateUserProfileR
  addToken
  addPostParam "about-me" $ unTextarea aboutMeUPF
  forM_ mImageFile $ addTestFileWith "image"

updateUserProfile :: UserProfileForm -> Maybe TestFile -> YesodExample App ()
updateUserProfile form mImageFile = do
  testCanReach $ ProfileR UpdateUserProfileR
  updateUserProfileR form mImageFile
  statusIs 303
  locationShouldBe $ ProfileR ProfilePageR
  _ <- followRedirect
  statusIs 200

testUpdateUserProfile :: UserProfileForm -> YesodExample App ()
testUpdateUserProfile form = do
  number <- liftIO $ randomRIO (0 :: Double, 1)
  mImageFile <-
    if number > 0.5
      then Just <$> readTestFile "test-resources/coach/paul.jpg"
      else pure Nothing
  updateUserProfile form mImageFile

updateCoachProfileR :: CoachProfileForm -> YesodExample App ()
updateCoachProfileR CoachProfileForm {..} = request $ do
  setMethod methodPost
  setUrl $ ProfileR UpdateCoachProfileR
  addToken
  addPostParam "expertise" $ unTextarea expertiseCPF

updateCoachProfile :: CoachProfileForm -> YesodExample App ()
updateCoachProfile form = do
  testCanReach $ ProfileR ProfilePageR
  updateCoachProfileR form
  statusIs 303
  locationShouldBe $ ProfileR ProfilePageR
  _ <- followRedirect
  statusIs 200

testUpdateCoachProfile :: CoachProfileForm -> YesodExample App ()
testUpdateCoachProfile = updateCoachProfile

testDB :: DB.SqlPersistT IO a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runSqlPool func pool

shouldBeSort :: (Ord a, Show a) => [a] -> [a] -> IO ()
xs `shouldBeSort` ys = sort xs `shouldBe` sort ys

testSendConnectionProposal :: Coach -> YesodExample App ()
testSendConnectionProposal coach = do
  get $ ProfileR ListCoachesR
  post . ProfileR . ConnectCoachR $ coachUuid coach
  statusIs 303
  locationShouldBe $ ProfileR ListCoachesR
  _ <- followRedirect
  statusIs 200

testRespondToProposal :: UserUUID -> CoachCoachProposalResponse -> YesodExample App ()
testRespondToProposal user response = do
  get HomeR
  post . NewsfeedR $ ConnectCoachResponseR user response
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200

testSendFriendRequest :: User -> YesodExample App ()
testSendFriendRequest User {..} = do
  get $ ProfileR ListFriendsR
  post . ProfileR $ ConnectFriendR userUuid
  statusIs 303
  locationShouldBe $ ProfileR ListFriendsR
  _ <- followRedirect
  statusIs 200

testFriendResponse :: UserUUID -> FriendRequestResponse -> YesodExample App ()
testFriendResponse uuid response = do
  get HomeR
  post . NewsfeedR $ ConnectFriendResponseR uuid response
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200

addUserWorkoutRequest :: AddUserWorkoutForm -> WorkoutType -> YesodExample App ()
addUserWorkoutRequest AddUserWorkoutForm {..} workoutType = request $ do
  setMethod methodPost
  setUrl . NewsfeedR $ AddUserWorkoutR workoutType
  addToken
  addPostParam "amount" . T.pack . show $ (round $ amountAWF / stepSize workoutType :: Int)
  addPostParam "day" . T.pack $ show dayAWF

submitUserWorkout :: AddUserWorkoutForm -> WorkoutType -> YesodExample App ()
submitUserWorkout form workoutType = do
  testCanReach . NewsfeedR $ AddUserWorkoutR workoutType
  addUserWorkoutRequest form workoutType
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200

addCoachWorkoutRequest :: AddCoachWorkoutForm -> WorkoutType -> YesodExample App ()
addCoachWorkoutRequest AddCoachWorkoutForm {..} workoutType = request $ do
  setMethod methodPost
  setUrl . ActivitiesR $ AddCoachWorkoutR workoutType
  addToken
  addPostParam "amount" . T.pack . show $ (round $ amountACWF / stepSize workoutType :: Int)
  addPostParam "day" . T.pack $ show dayACWF
  addPostParam "notes" $ unTextarea notesACWF

submitCoachWorkout :: AddCoachWorkoutForm -> WorkoutType -> YesodExample App ()
submitCoachWorkout form workoutType = do
  testCanReach . ActivitiesR $ AddCoachWorkoutR workoutType
  addCoachWorkoutRequest form workoutType
  statusIs 303
  locationShouldBe $ ActivitiesR ActivitiesPageR
  _ <- followRedirect
  statusIs 200

joinWorkout :: CoachWorkoutUUID -> YesodExample App ()
joinWorkout uuid = do
  testCanReach $ ActivitiesR ActivitiesPageR
  post . ActivitiesR $ JoinCoachWorkoutR uuid
  statusIs 303
  locationShouldBe $ ActivitiesR ActivitiesPageR
  _ <- followRedirect
  statusIs 200

cancelWorkout :: CoachWorkoutUUID -> YesodExample App ()
cancelWorkout uuid = do
  testCanReach $ ActivitiesR ActivitiesPageR
  post . ActivitiesR $ CancelCoachWorkoutJoinR uuid
  statusIs 303
  locationShouldBe $ ActivitiesR ActivitiesPageR
  _ <- followRedirect
  statusIs 200

getSingleUser :: YesodExample App User
getSingleUser =
  testDB (selectList [] []) >>= \case
    [Entity _ user] -> pure user
    xs -> fail $ "Found " <> show (length xs) <> " users instead of 1"

getSingleCoach :: YesodExample App Coach
getSingleCoach =
  testDB (selectList [] []) >>= \case
    [Entity _ coach] -> pure coach
    xs -> fail $ "Found " <> show (length xs) <> " coachs instead of 1"

getSingleCoachWorkout :: YesodExample App CoachWorkout
getSingleCoachWorkout =
  testDB (selectList [] []) >>= \case
    [Entity _ workout] -> pure workout
    xs -> fail $ "Found " <> show (length xs) <> " coach workouts instead of 1"

getSingleWorkoutJoin :: YesodExample App WorkoutJoin
getSingleWorkoutJoin =
  testDB (selectList [] []) >>= \case
    [Entity _ workoutJoin] -> pure workoutJoin
    xs -> fail $ "Found " <> show (length xs) <> " workout joins instead of 1"

testRequiresLogin :: String -> Route App -> PactWebServerSpec
testRequiresLogin routeString route = do
  it "GETs 200 if logged in" $ \yc -> do
    forAllValid $ \testUser -> runYesodClientM yc $ do
      testRegisterUser testUser
      testCanReach route

  it "GETs 303 redirect to Login if not logged in" $ \yc ->
    runYesodClientM yc $ testCannotReach route

  it ("GET without logged in, then follow the redirect, ends up at " <> routeString) $
    \yc -> forAllValid $ \testUser -> runYesodClientM yc $ do
      testRegisterUser testUser
      testLogout
      testCannotReach route
      _ <- followRedirect
      loginRequest (testUsername testUser) $ testUserPassword testUser
      statusIs 303
      locationShouldBe route
      _ <- followRedirect
      statusIs 200

testRequiresCoach :: String -> Route App -> PactWebServerSpec
testRequiresCoach routeString route = do
  it "GETs 403 if logged in as non-coach user" $ \yc -> do
    forAllValid $ \testUser -> runYesodClientM yc $ do
      testRegisterUser testUser
      get route
      statusIs 403

  it "GETs 200 if logged in coach" $ \yc -> do
    forAllValid $ \testCoach -> runYesodClientM yc $ do
      testRegisterUser testCoach
      becomeCoach
      testCanReach route

  it "GETs 303 redirect to Login if not logged in" $ \yc ->
    runYesodClientM yc $ testCannotReach route

  it ("GET without logged in, then follow the redirect, ends up at " <> routeString) $
    \yc -> forAllValid $ \testCoach -> runYesodClientM yc $ do
      testRegisterUser testCoach
      becomeCoach
      testLogout
      testCannotReach route
      _ <- followRedirect
      loginRequest (testUsername testCoach) $ testUserPassword testCoach
      statusIs 303
      locationShouldBe route
      _ <- followRedirect
      statusIs 200
