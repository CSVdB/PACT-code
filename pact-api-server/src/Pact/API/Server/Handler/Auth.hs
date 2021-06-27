{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.API.Server.Handler.Auth where

import Control.Monad.IO.Class
import Data.Password.Bcrypt
import qualified Data.Text.Encoding as TE
import Debug.Trace (trace)
import Pact.API.Server.Handler.Import
import Servant.Auth.Server (makeSessionCookieBS)

type ProfileWithCookie = Headers '[Header "Set-Cookie" Text] Profile

profileWithCookie :: User -> H ProfileWithCookie
profileWithCookie user = do
  cookieSettings <- asks envCookieSettings
  jwtSettings <- asks envJWTSettings
  mCookie <- liftIO $ makeSessionCookieBS cookieSettings jwtSettings authCookie
  case mCookie of
    Nothing -> throwError err401
    Just setCookie -> pure $ addHeader (TE.decodeUtf8 setCookie) $ toProfile user
  where
    authCookie = AuthCookie {authCookieUsername = userName user}

handlePostRegister :: RegistrationForm -> H ProfileWithCookie
handlePostRegister reg@RegistrationForm {..} = do
  mUser <- getUser registrationFormUsername
  case mUser of
    Just _ -> throwError err409
    Nothing -> do
      user <- registrationToUser reg
      runDB $ insert_ user
      profileWithCookie user

handlePostLogin :: LoginForm -> H ProfileWithCookie
handlePostLogin LoginForm {..} = do
  mUser <- getUser loginFormUsername
  case mUser of
    Nothing -> trace "Non-existent" $ throwError err401 -- Not 404, because then we leak data about users.
    Just (Entity _ user@User {..}) ->
      case checkPassword (mkPassword loginFormPassword) userPassword of
        PasswordCheckFail -> trace "Wrong password" $ throwError err401
        PasswordCheckSuccess -> do
          let authCookie =
                AuthCookie {authCookieUsername = loginFormUsername}
          cookieSettings <- asks envCookieSettings
          jwtSettings <- asks envJWTSettings
          mCookie <-
            liftIO $
              makeSessionCookieBS
                cookieSettings
                jwtSettings
                authCookie
          case mCookie of
            Nothing -> throwError err401
            Just setCookie ->
              return $
                addHeader (TE.decodeUtf8 setCookie) $ toProfile user

handleGetUser :: AuthCookie -> H Profile
handleGetUser AuthCookie {..} = withUser authCookieUsername pure
