{-# LANGUAGE OverloadedStrings #-}

module Pact.Web.Server.Handler.VideoSpec (spec) where

import Data.UUID.V4 (nextRandom)
import qualified Database.Persist.Class as P
import qualified Database.Persist.Types as P
import Pact.DB
import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec . describe "Video" $ do
  describe "VideoR" $ do
    it "POST an exercise actually adds an video" $ \yc ->
      forAllValid $ \testUser -> forAllValid $ \form -> runYesodClientM yc $ do
        testRegisterUser testUser
        testSubmitExercise form
        videos <- testDB $ P.selectList [] [] -- Collect all videos
        liftIO $ (videos :: [P.Entity Video]) `shouldNotBe` []
    it "Can GET an existent video" $ \yc ->
      forAllValid $ \video ->
        runYesodClientM yc $ do
          testDB $ P.insert_ video
          get $ VideoR $ videoUuid video
          statusIs 200
    it "GETs a 404 for a nonexistent video" $ do
      uuid <- liftIO nextRandom
      get $ VideoR uuid
      statusIs 404
