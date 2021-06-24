module PACT.Component.HTML.Utils where

import Prelude
import PACT.Data.Router (Route, routeCodec)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

-- Utility we use a lot, so a short helper function is useful.
css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

-- Function based on the HTML `href` used to create a hyperlink to another route
-- within our application.
safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem Nothing _ = HH.text ""

whenElem :: forall p i. Boolean -> HH.HTML p i -> HH.HTML p i
whenElem true html = html
whenElem false _ = HH.text ""
