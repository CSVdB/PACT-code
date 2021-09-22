{-# LANGUAGE OverloadedStrings #-}

module Pact.Web.Server.Handler.ImageSpec (spec) where

import Data.UUID.Typed (nextRandomUUID)
import qualified Database.Persist.Class as P
import qualified Database.Persist.Types as P
import Pact.DB
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "Image" $ do
  describe "ImageR" $ do
    it "POST an exercise actually adds an image" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \exerciseForm ->
        runYesodClientM yc $ do
          testRegisterUser testUser
          becomeCoach
          testSubmitExercise exerciseForm
          images <- testDB $ P.selectList [] [] -- Collect all images
          liftIO $ (images :: [P.Entity Image]) `shouldNotBe` []
    it "Can GET an existent image" $ \yc ->
      forAllValid $ \image ->
        runYesodClientM yc $ do
          testDB $ P.insert_ image
          get $ ImageR $ imageUuid image
          statusIs 200
    it "GETs a 404 for a nonexistent image" $ do
      uuid <- liftIO nextRandomUUID
      get $ ImageR uuid
      statusIs 404
