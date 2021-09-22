{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Pact.DB.Persistent where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist.Sql

selectListVals ::
  forall record backend m.
  (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) =>
  [Filter record] ->
  [SelectOpt record] ->
  ReaderT backend m [record]
selectListVals filters opts = fmap entityVal <$> selectList filters opts
