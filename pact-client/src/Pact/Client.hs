module Pact.Client
  ( module Pact.Client,
    module Pact.API,
    module X,
  )
where

import Pact.API
import Servant.API as X
import Servant.Auth.Client as X
import Servant.Client as X
import Servant.Client.Generic

pactClient :: PactRoutes (AsClientT ClientM)
pactClient = genericClient
