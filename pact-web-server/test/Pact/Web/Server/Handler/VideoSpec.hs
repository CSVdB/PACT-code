{-# LANGUAGE OverloadedStrings #-}

module Pact.Web.Server.Handler.VideoSpec (spec) where

import Data.UUID.Typed (nextRandomUUID)
import qualified Database.Persist.Class as P
import qualified Database.Persist.Types as P
import Pact.DB
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "Video" $ do
  describe "VideoR" $ do
    it "POST an exercise actually adds an video" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \profileForm ->
        forAllValid $ \exerciseForm -> runYesodClientM yc $ do
          testRegisterUser testUser
          testProfileUpsert profileForm
          testSubmitExercise exerciseForm
          videos <- testDB $ P.selectList [] [] -- Collect all videos
          liftIO $ (videos :: [P.Entity Video]) `shouldNotBe` []
    it "Can GET an existent video" $ \yc ->
      forAllValid $ \video ->
        runYesodClientM yc $ do
          testDB $ P.insert_ video
          get $ VideoR $ videoUuid video
          statusIs 200
    it "GETs a 404 for a nonexistent video" $ do
      uuid <- liftIO nextRandomUUID
      get $ VideoR uuid
      statusIs 404
