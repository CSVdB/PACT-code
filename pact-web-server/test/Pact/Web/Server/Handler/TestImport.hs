module Pact.Web.Server.Handler.TestImport (module X, genNewJoinStatus) where

import Control.Monad as X
import Pact.DB as X
import Pact.Data as X
import Pact.Web.Server.Foundation as X
import Pact.Web.Server.Gen as X
import Pact.Web.Server.PathPiece as X
import Pact.Web.Server.TestUtils as X
import Test.QuickCheck as X
import Test.Syd as X
import Test.Syd.Validity as X
import Test.Syd.Validity.Aeson as X
import Test.Syd.Validity.Persist as X
import Test.Syd.Yesod as X
import Yesod.Auth as X

-- Generate a join status other than WillCome.
genNewJoinStatus :: Gen JoinStatus
genNewJoinStatus = genValid `suchThat` (/= WillCome)
