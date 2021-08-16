{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.TestUtils where

import Control.Lens ((&), (.~))
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Database.Persist.Sql as DB
import Database.Persist.Sqlite (fkEnabled, mkSqliteConnectionInfo, runMigration, runSqlPool, walEnabled, withSqlitePoolInfo)
import GHC.Generics
import Network.HTTP.Client as HTTP
import Pact.DB
import Pact.Data
import Pact.Web.Server.Application ()
import Pact.Web.Server.Foundation
import Pact.Web.Server.Gen
import Pact.Web.Server.Handler hiding (get)
import Path.IO
import System.FilePath (takeExtension)
import Test.Syd
import Test.Syd.Path
import Test.Syd.Wai (managerSpec)
import Test.Syd.Yesod

type PactWebServerSpec = YesodSpec App

pactWebServerSpec :: PactWebServerSpec -> Spec
pactWebServerSpec = modifyMaxSuccess (`div` 20) . managerSpec . yesodSpecWithSiteSetupFunc serverSetup

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
    runSqlPool (runMigration serverMigration) pool
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
        _ -> Nothing
  pure TestFile {..}

addExerciseRequest :: AddExerciseForm -> TestFile -> YesodExample App ()
addExerciseRequest AddExerciseForm {..} TestFile {..} = request $ do
  setMethod methodPost
  setUrl $ ExerciseR AddR
  addToken
  addPostParam "exerciseName" nameEF
  addPostParam "difficulty" . T.pack $ show difficultyEF
  addPostParam "formTips" $ unTextarea formTipsEF
  addPostParam "notes" $ maybe "" unTextarea notesEF
  addFileWith "image" testFilePath testFileContents testFileType

submitExercise :: AddExerciseForm -> TestFile -> YesodExample App ()
submitExercise form file = do
  testCanReach $ ExerciseR AddR
  addExerciseRequest form file
  statusIs 303
  getLocation >>= \case
    Left err -> fail $ T.unpack err
    Right (ExerciseR (ViewR _)) -> do
      _ <- followRedirect
      statusIs 200
    Right _ -> fail "Redirect after submitting an exercise ends up in the wrong location"

testDB :: DB.SqlPersistT IO a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runSqlPool func pool
