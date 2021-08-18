{-# LANGUAGE FlexibleContexts #-}

module Pact.Web.Server.Yesod where

import qualified Data.Text as T
import Yesod

optionsEnumShow :: (MonadHandler m, Show a, Enum a, Bounded a) => m (OptionList a)
optionsEnumShow = optionsPairsShow $ map (\x -> (T.pack $ show x, x)) [minBound .. maxBound]

-- | Creates an 'OptionList' from a list of (display-value, internal value)
-- pairs using `Show` for the external value.
optionsPairsShow ::
  (MonadHandler m, RenderMessage (HandlerSite m) msg, Show a) =>
  [(msg, a)] ->
  m (OptionList a)
optionsPairsShow opts = do
  mr <- getMessageRender
  let mkOption (display, internal) =
        Option
          { optionDisplay = mr display,
            optionInternalValue = internal,
            optionExternalValue = T.pack $ show internal
          }
  pure . mkOptionList $ mkOption <$> opts
