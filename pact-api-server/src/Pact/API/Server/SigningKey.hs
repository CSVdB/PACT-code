module Pact.API.Server.SigningKey
  ( loadSigningKey,
  )
where

import Crypto.JOSE.JWK (JWK)
import Path
import Path.IO
import qualified Servant.Auth.Server as Auth

loadSigningKey :: Path Abs File -> IO JWK
loadSigningKey skf = do
  mErrOrKey <- forgivingAbsence $ Auth.readKey (toFilePath skf)
  case mErrOrKey of
    Nothing -> do
      -- Generate 256 random bytes, call that the key, and write it to the file.
      Auth.writeKey $ fromAbsFile skf
      Auth.readKey $ fromAbsFile skf
    Just r -> pure r
