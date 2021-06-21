module PACT.Capability.User where

import Prelude
import PACT.Data.User
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageUser m where
  loginUser :: LoginForm -> m (Maybe Profile)
  registerUser :: RegistrationForm -> m (Maybe Profile)
  getCurrentUser :: m (Maybe Profile)

instance ManageUser m => ManageUser (HalogenM st act slots out m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser
