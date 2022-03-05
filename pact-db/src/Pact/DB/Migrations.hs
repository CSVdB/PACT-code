{-# LANGUAGE OverloadedStrings #-}

module Pact.DB.Migrations where

import Control.Monad
import Control.Monad.IO.Class
import Data.List ((\\))
import Data.Text (Text)
import Data.UUID.Typed (nextRandomUUID)
import Database.Persist.Sql
import Pact.DB

appSpecificMigrations :: SqlPersistT IO ()
appSpecificMigrations = setInitialExerciseMaterials

exerciseMaterialNames :: [Text]
exerciseMaterialNames =
  [ "TRX",
    "Pullup bar",
    "Dip bars",
    "Parallites",
    "Elastics",
    "Swimming pool"
    -- TODO: Add "Soft surface for jogging" (once you think of a good name)
  ]

setInitialExerciseMaterials :: SqlPersistT IO ()
setInitialExerciseMaterials = do
  existingMaterialNames <- fmap exerciseMaterialName <$> collectAllMaterials
  let newNames = exerciseMaterialNames \\ existingMaterialNames
  forM_ newNames $ \name -> do
    uuid <- liftIO nextRandomUUID
    insert_
      ExerciseMaterial
        { exerciseMaterialUuid = uuid,
          exerciseMaterialName = name
        }
