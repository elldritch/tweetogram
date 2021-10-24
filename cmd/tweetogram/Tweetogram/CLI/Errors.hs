module Tweetogram.CLI.Errors (
  handleExceptions,
  showExceptions,
) where

import Relude

handleExceptions ::
  (MonadIO m) =>
  (SomeException -> m (Maybe String)) ->
  Either SomeException a ->
  m a
handleExceptions handlers result =
  case result of
    Right r -> pure r
    Left exc -> do
      msg <- handlers exc
      die $ fromMaybe ("Unexpected error: " <> displayException exc) msg

showExceptions ::
  (MonadIO m) =>
  Either SomeException a ->
  (SomeException -> m (Maybe String)) ->
  m a
showExceptions = flip handleExceptions
