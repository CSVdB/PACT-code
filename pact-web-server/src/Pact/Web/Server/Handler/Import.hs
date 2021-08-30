{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Handler.Import (module X, Generic) where

import Control.Monad as X
import Data.Text as X (Text)
import Data.Validity as X
import GHC.Generics (Generic)
import Pact.DB as X
import Pact.Data as X
import Pact.Web.Server.Foundation as X
import Pact.Web.Server.Static as X
import Pact.Web.Server.Widget as X
import Pact.Web.Server.Yesod as X
import Yesod as X hiding (check)
import Yesod.Auth as X

instance X.Validity Textarea where
  validate (Textarea t) = delve "Textarea" t
