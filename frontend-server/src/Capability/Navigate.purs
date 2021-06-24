module PACT.Capability.Navigate where

import Prelude
import PACT.Data.Router (Route)
import Halogen (HalogenM, lift)

class Monad m <= Navigate m where
  navigate :: Route -> m Unit
  logoutUser :: m Unit

instance Navigate m => Navigate (HalogenM st act slots out m) where
  navigate = lift <<< navigate
  logoutUser = lift logoutUser
