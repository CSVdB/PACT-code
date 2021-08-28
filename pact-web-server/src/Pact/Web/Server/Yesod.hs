{-# LANGUAGE FlexibleContexts #-}

module Pact.Web.Server.Yesod where

import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Yesod

optionsEnumShow ::
  (MonadHandler m, Show a, Enum a, Bounded a) => m (OptionList a)
optionsEnumShow = optionsShowList [minBound .. maxBound]

optionsShowList :: (MonadHandler m, Show a) => [a] -> m (OptionList a)
optionsShowList = optionsPairsShow . (fmap $ \x -> (T.pack $ show x, x))

optionsPairsWithExternal ::
  (MonadHandler m, RenderMessage (HandlerSite m) msg) =>
  [(msg, a, Text)] ->
  m (OptionList a)
optionsPairsWithExternal opts = do
  mr <- getMessageRender
  let mkOption (display, internal, external) =
        Option
          { optionDisplay = mr display,
            optionInternalValue = internal,
            optionExternalValue = external
          }
  pure $ mkOptionList $ mkOption <$> opts

-- | Creates an 'OptionList' from a list of (display-value, internal value)
-- pairs using `Show` for the external value.
optionsPairsShow ::
  (MonadHandler m, RenderMessage (HandlerSite m) msg, Show a) =>
  [(msg, a)] ->
  m (OptionList a)
optionsPairsShow opts =
  optionsPairsWithExternal $
    opts <&> \(msg, internal) ->
      (msg, internal, T.pack $ show internal)
