{-# LANGUAGE OverloadedStrings #-}

module Pact.DB.Migrations where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Text (Text)
import Data.UUID.Typed (nextRandomUUID)
import Database.Persist.Sql
import Pact.DB

allServerMigrations :: (MonadLogger m, MonadIO m) => SqlPersistT m ()
allServerMigrations = do
  logInfoN "Running automatic migration"
  runMigration serverMigration
  logInfoN "Running application specific migrations"
  appSpecificMigrations

appSpecificMigrations :: (MonadLogger m, MonadIO m) => SqlPersistT m ()
appSpecificMigrations = setInitialExerciseMaterials

exerciseMaterialNames :: [Text]
exerciseMaterialNames =
  [ "TRX",
    "PullupBar",
    "DipBars",
    "ThingForInclined",
    "Parallites",
    "Elastics",
    "Swimming pool"
    -- TODO: Add "Soft surface for jogging" (once you think of a good name)
  ]

setInitialExerciseMaterials :: (MonadLogger m, MonadIO m) => SqlPersistT m ()
setInitialExerciseMaterials = do
  logInfoN "Setting up initial exercise materials"
  forM_ exerciseMaterialNames $ \name -> do
    uuid <- liftIO nextRandomUUID
    insert_
      ExerciseMaterial
        { exerciseMaterialUuid = uuid,
          exerciseMaterialName = name
        }
