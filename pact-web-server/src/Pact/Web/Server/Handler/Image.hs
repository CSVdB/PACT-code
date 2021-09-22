{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Image where

import qualified Data.Text.Encoding as TE
import Pact.Web.Server.Handler.Prelude

getImageR :: ImageUUID -> Handler TypedContent
getImageR uuid =
  runDB (getBy $ UniqueImageUUID uuid) >>= \case
    Nothing -> notFound
    Just (Entity _ Image {..}) ->
      -- TODO: Once we are using hash-based keys ("ImageUUID"), set the ETag
      -- (`setEtag`) and set the correct headers to use caching
      respond (TE.encodeUtf8 imageTyp) imageContents
