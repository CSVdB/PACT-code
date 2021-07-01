module PACT.Data.Router where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Login
  | Register
  | Greet

derive instance Generic Route _
derive instance Eq Route
derive instance Ord Route

-- | Bidirectional codec for route parsing. This will handle both parsing
-- | browser locations and serializing our data type to a browser location.
-- | Causes a compile-error if any of the routes is missing.
routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "Home": noArgs
        , "Login": "login" / noArgs
        , "Register": "register" / noArgs
        , "Greet": "greet" / noArgs
        }
