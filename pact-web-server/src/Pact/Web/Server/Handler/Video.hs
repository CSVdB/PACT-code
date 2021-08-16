{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Video where

import qualified Data.Text.Encoding as TE
import Pact.Web.Server.Handler.Import

getVideoR :: VideoUUID -> Handler TypedContent
getVideoR uuid = do
  mVideo <- runDB . getBy $ UniqueVideoUUID uuid
  case mVideo of
    Nothing -> notFound
    Just (Entity _ Video {..}) -> do
      -- TODO: Once we are using hash-based keys ("VideoUUID"), set the ETag
      -- (`setEtag`) and set the correct headers to use caching
      respond (TE.encodeUtf8 videoTyp) videoContents
