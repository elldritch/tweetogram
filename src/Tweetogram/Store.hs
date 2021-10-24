-- | Provide functionality for storing and retrieving folders of NDJSON.
module Tweetogram.Store (
  storePages,
  load,
  ParseException (..),
) where

import Conduit (mapMC)
import Control.Exception (throwIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.Conduit (ConduitM, ConduitT, (.|))
import Data.Conduit.Combinators (
  concatMapE,
  sinkFileCautious,
  sourceFile,
  splitOnUnboundedE,
 )
import Data.MonoTraversable (Element, MonoFoldable)
import Relude

-- | Store a monofoldable page of items by serializing to JSON and saving it in
-- a newline-delimited file.
storePages ::
  (MonadResource m, MonoFoldable i, ToJSON (Element i)) =>
  FilePath ->
  ConduitM i o m ()
storePages filename =
  concatMapE ((<> "\n") . toStrict . encode)
    .| sinkFileCautious filename

-- | Use a newline-delimited JSON file as a source, emitting each line.
load :: (MonadResource m, FromJSON o) => FilePath -> ConduitM i o m ()
load filename =
  sourceFile filename
    .| splitOnUnboundedE (== (toEnum $ ord '\n'))
    .| decode
 where
  decode :: (MonadIO m, FromJSON o) => ConduitT ByteString o m ()
  decode = mapMC $ \line -> do
    case eitherDecodeStrict' line of
      Left err -> liftIO $ throwIO $ ParseException filename err
      Right item -> pure item

data ParseException = ParseException
  { filename :: FilePath
  , errorMessage :: String
  }
  deriving (Show)

instance Exception ParseException where
  displayException (ParseException file msg) =
    "could not parse NDJSON at " <> show file <> ": " <> msg
