{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Video where

import qualified Data.Text.Encoding as TE
import Pact.Web.Server.Handler.Prelude

getVideoR :: VideoUUID -> Handler TypedContent
getVideoR uuid =
  runDB (getBy $ UniqueVideoUUID uuid) >>= \case
    Nothing -> notFound
    Just (Entity _ Video {..}) ->
      -- TODO: Once we are using hash-based keys ("VideoUUID"), set the ETag
      -- (`setEtag`) and set the correct headers to use caching
      respond (TE.encodeUtf8 videoTyp) videoContents
