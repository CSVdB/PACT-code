module PACT.Capability.Now where

import Prelude
import Data.DateTime (DateTime)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

class
  Monad m <= Now m where
  nowDateTime :: m DateTime

instance Now m => Now (HalogenM st act slots msg m) where
  nowDateTime = lift nowDateTime
