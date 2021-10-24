module Tweetogram.CLI.Store (
  fmtStoreErr,
  fmtLoadErr,
) where

import Relude

import GHC.IO.Exception (IOErrorType (..), IOException (..))

fmtStoreErr :: IOException -> Maybe String
fmtStoreErr (IOError _ NoSuchThing _ description _ (Just filename)) =
  Just $ "Could not download tweets to " <> show filename <> ": " <> description
fmtStoreErr _ = Nothing

fmtLoadErr :: IOException -> Maybe String
fmtLoadErr (IOError _ NoSuchThing _ description _ (Just filename)) =
  Just $ "Could not load tweets from " <> show filename <> ": " <> description
fmtLoadErr _ = Nothing
