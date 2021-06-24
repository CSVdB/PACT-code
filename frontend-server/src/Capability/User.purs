module PACT.Capability.User where

import Prelude (class Monad, (<<<))
import PACT.Data.User
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Maybe Profile)
  getCurrentUser :: m (Maybe Profile)

instance ManageUser m => ManageUser (HalogenM st act slots out m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser
