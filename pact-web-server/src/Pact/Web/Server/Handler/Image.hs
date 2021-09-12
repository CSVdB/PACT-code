{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Image where

import qualified Data.Text.Encoding as TE
import Pact.Web.Server.Handler.Prelude

getImageR :: ImageUUID -> Handler TypedContent
getImageR uuid = do
  mImage <- runDB . getBy $ UniqueImageUUID uuid
  case mImage of
    Nothing -> notFound
    Just (Entity _ Image {..}) -> do
      -- TODO: Once we are using hash-based keys ("ImageUUID"), set the ETag
      -- (`setEtag`) and set the correct headers to use caching
      respond (TE.encodeUtf8 imageTyp) imageContents
