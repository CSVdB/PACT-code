{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Handler.Profile.ListCurrentCustomers where

import Pact.Web.Server.Handler.Prelude

getListCurrentCustomersR :: Handler Html
getListCurrentCustomersR = do
  token <- genToken
  (_, mCoach) <- getCoachM
  customers <- fmap (fromMaybe []) $ forM mCoach $ runDB . collectCustomers
  defaultLayout $ do
    setTitleI ("Customers" :: Text)
    $(widgetFile "profile/listCurrentCustomers")
