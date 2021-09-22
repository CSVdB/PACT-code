{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Foundation where

import Data.FileEmbed (makeRelativeToProject)
import Data.Functor
import Data.Text (Text)
import Data.UUID.Typed (nextRandomUUID)
import Database.Persist.Sql
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Pact.DB
import Pact.Data
import Pact.Web.Server.Constants
import Pact.Web.Server.Static
import Pact.Web.Server.Widget
import Path
import Text.Hamlet
import Yesod
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.AutoReload
import Yesod.EmbeddedStatic

data App = App
  { appLogLevel :: !LogLevel,
    appStatic :: !EmbeddedStatic,
    appHTTPManager :: !HTTP.Manager,
    appConnectionPool :: !ConnectionPool,
    appSessionKeyFile :: !(Path Abs File),
    appGoogleAnalyticsTracking :: !(Maybe Text),
    appGoogleSearchConsoleVerification :: !(Maybe Text)
  }

mkYesodData "App" $(makeRelativeToProject "routes.txt" >>= parseRoutesFile)

instance Yesod App where
  defaultLayout widget = do
    app <- getYesod
    messages <- getMessages
    currentRoute <- getCurrentRoute
    let withAutoReload =
          if development
            then (<> autoReloadWidgetFor ReloadR)
            else id
    navbarRoutes <- navbarRoutesH
    navbarPC <- widgetToPageContent $(widgetFile "navbar")
    pageContent <- widgetToPageContent $ withAutoReload $(widgetFile "default-body")
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")

  makeSessionBackend a = Just <$> defaultClientSessionBackend (60 * 24 * 365 * 10) (fromAbsFile (appSessionKeyFile a))

  shouldLogIO app _ ll = pure $ ll >= appLogLevel app

  maximumContentLength _ route = case route of
    -- Remove any limit for routes which add images or videos.
    Just (ExerciseR AddR) -> Just $ 20 * 1024 * 1024 -- 20MB
    _ -> Just $ 2 * 1024 * 1024 -- 2MB

  authRoute _ = Just $ AuthR LoginR

  -- Split off AccountR, CoachR and AdminR.
  -- List each route explicitly to avoid mistakes.
  isAuthorized route _ = do
    userType <- getUserType
    case route of
      NewsfeedR _ -> requireUser userType
      ActivitiesR (AddCoachWorkoutR _) -> requireCoach userType
      ActivitiesR _ -> requireUser userType
      ProfileR UpdateCoachProfileR -> requireCoach userType
      ProfileR _ -> requireUser userType
      ExerciseR AddR -> requireCoach userType
      ExerciseR _ -> requireUser userType
      _ -> pure Authorized
    where
      -- Must be logged in as some type of user
      requireUser = \case
        Nobody -> pure AuthenticationRequired
        LoggedInUser _ -> pure Authorized
        LoggedInCoach _ _ -> pure Authorized
      requireCoach = \case
        Nobody -> pure AuthenticationRequired
        LoggedInUser _ -> pure $ Unauthorized "You must be a coach to access this page."
        LoggedInCoach _ _ -> pure Authorized

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB func = do
    pool <- getsYesod appConnectionPool
    runSqlPool func pool

instance YesodAuth App where
  type AuthId App = UserId
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authenticate Creds {..} = case credsPlugin of
    "impersonation" -> byUser
    "pact" -> byUser -- Our self-defined Auth plugin name
    _ -> pure $ ServerError "Unknown auth plugin"
    where
      byUser = do
        mUser <- case parseUsername credsIdent of
          Left _ -> pure Nothing
          Right un -> liftHandler . runDB . getBy $ UniqueUsername un
        pure $ case mUser of
          Nothing -> UserError $ IdentifierNotFound credsIdent
          Just (Entity userId _) -> Authenticated userId
  onLogin = pure () -- addMessageI "is-success" NowLoggedIn
  authPlugins _ = [pactAuthPlugin]

instance YesodAuthPersist App

genToken :: MonadHandler m => m Html
genToken = do
  alreadyExpired
  req <- getRequest
  let tokenKey = defaultCsrfParamName
  pure $
    case reqToken req of
      Nothing -> mempty
      Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]

getFaviconR :: Handler TypedContent
getFaviconR = redirect $ StaticR logo_jpg

data UserType
  = Nobody
  | LoggedInUser User
  | LoggedInCoach User Coach
  -- TODO: Add LoggedInAdmin
  deriving (Show, Eq, Ord, Generic)

getUserType :: Handler UserType
getUserType = do
  mAuth <- maybeAuth
  case mAuth of
    Nothing -> pure Nobody
    Just (Entity _ user) -> do
      mCoach <- runDB . getBy . UniqueCoachUser $ userUuid user
      case mCoach of
        Nothing -> pure $ LoggedInUser user
        Just (Entity _ coach) -> pure $ LoggedInCoach user coach

-- TODO: Once admin user is created, add that options here
navbarRoutesH :: Handler [(Route App, String)]
navbarRoutesH =
  getUserType <&> \userType -> specificNavbarRoutes userType
  where
    specificNavbarRoutes = \case
      Nobody -> navbarRoutesNobody
      LoggedInUser _ -> navbarRoutesUser
      LoggedInCoach _ _ -> navbarRoutesCoach

navbarRoutesNobody :: [(Route App, String)]
navbarRoutesNobody =
  [ (HomeR, "Home"),
    (AuthR LoginR, "Log in"),
    (AuthR registerR, "Sign up")
  ]

navbarRoutesUser :: [(Route App, String)]
navbarRoutesUser =
  [ (HomeR, "Newsfeed"),
    (ActivitiesR ActivitiesPageR, "Activities"),
    (ProfileR ProfilePageR, "Profile")
    -- (ExerciseR ViewAllR, "Exercises"),
    -- (ExerciseR AddR, "Add exercise"),
  ]

navbarRoutesCoach :: [(Route App, String)]
navbarRoutesCoach = navbarRoutesUser

getReloadR :: Handler ()
getReloadR = getAutoReloadR

pactAuthPluginName :: Text
pactAuthPluginName = "pact"

pactLoginHandler :: (Route Auth -> Route App) -> Widget
pactLoginHandler _toParentRoute = do
  messages <- getMessages
  token <- genToken
  setTitle "Login"
  $(widgetFile "auth/login")

pactAuthPlugin :: AuthPlugin App
pactAuthPlugin = AuthPlugin pactAuthPluginName dispatch pactLoginHandler
  where
    dispatch "GET" ["register"] = getRegisterR >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR
    dispatch "POST" ["login"] = postLoginR
    dispatch _ _ = notFound

registerR :: Route Auth
registerR = PluginR pactAuthPluginName ["register"]

loginR :: Route Auth
loginR = PluginR pactAuthPluginName ["login"]

getRegisterR :: AuthHandler App Html
getRegisterR = do
  messages <- getMessages
  token <- genToken
  liftHandler . defaultLayout $ do
    setTitle "Sign up"
    $(widgetFile "auth/register")

usernameField :: Field Handler Username
usernameField = checkMMap toUsername usernameText textField
  where
    toUsername :: Text -> Handler (Either FormMessage Username)
    toUsername t = pure $ mapLeft MsgInvalidEntry $ parseUsername t

registerForm :: FormInput Handler RegisterForm
registerForm =
  RegisterForm
    <$> ireq usernameField "username"
    <*> (mkPassword <$> ireq passwordField "password")
    <*> (mkPassword <$> ireq passwordField "confirmPassword")

postRegisterR :: AuthHandler App TypedContent
postRegisterR = liftHandler $ do
  rf@RegisterForm {..} <- runInputPost registerForm
  mUser <- runDB . getBy $ UniqueUsername registerFormUsername
  case mUser of
    Just _ -> do
      setMessage "Account already exists!"
      redirect $ AuthR registerR
    Nothing ->
      if not $ confirmPasswords rf
        then do
          addMessageI "is-danger" PassMismatch
          redirect $ AuthR registerR
        else do
          passphraseHash <- liftIO $ hashPassword registerFormPassword
          uuid <- nextRandomUUID
          runDB . insert_ $
            User
              { userUuid = uuid,
                userName = registerFormUsername,
                userPassword = passphraseHash,
                userPic = Nothing,
                userAboutMe = Textarea ""
              }
          setCredsRedirect
            Creds
              { credsPlugin = pactAuthPluginName,
                credsIdent = usernameText registerFormUsername,
                credsExtra = []
              }

loginForm :: FormInput Handler LoginForm
loginForm =
  LoginForm
    <$> ireq usernameField "username"
    <*> (mkPassword <$> ireq passwordField "password")

postLoginR :: AuthHandler App TypedContent
postLoginR = do
  LoginForm {..} <- liftHandler $ runInputPost loginForm
  mUser <- liftHandler $ runDB $ getBy (UniqueUsername loginFormUsername)
  case mUser of
    Nothing -> loginFail
    Just (Entity _ User {..}) ->
      case checkPassword loginFormPassword userPassword of
        PasswordCheckSuccess ->
          setCredsRedirect
            Creds
              { credsPlugin = pactAuthPluginName,
                credsIdent = usernameText loginFormUsername,
                credsExtra = []
              }
        PasswordCheckFail -> loginFail
  where
    loginFail = loginErrorMessageI LoginR InvalidLogin
