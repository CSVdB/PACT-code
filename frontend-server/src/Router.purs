module PACT.Router where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Login
  | Register

derive instance Generic Route _
derive instance Eq Route
derive instance Ord Route

-- | Next, we'll define a bidirectional codec for our route parsing. Our single
-- | codec will handle both parsing browser locations and serializing our data
-- | type to a browser location. We'll skip the boilerplate of separate encoding
-- | and decoding functions, and we'll ensure our parsing and printing is always
-- | in sync.
-- |
-- | Our codec will cause a compile-time error if we fail to handle any of our
-- | route cases.
routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "Home": noArgs
        , "Login": "login" / noArgs
        , "Register": "register" / noArgs
        }
