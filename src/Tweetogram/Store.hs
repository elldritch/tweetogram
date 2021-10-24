-- | Store and retrieve conduits of elements as NDJSON.
module Tweetogram.Store (
  store,
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
  sinkFileCautious,
  sourceFile,
  splitOnUnboundedE,
 )
import Data.Conduit.Combinators qualified as C
import Data.MonoTraversable (Element, MonoFoldable)
import Relude

-- | Store items by serializing them to NDJSON one item at a time.
store :: (MonadResource m, ToJSON i) => FilePath -> ConduitM i o m ()
store filename =
  C.map ((<> "\n") . toStrict . encode)
    .| sinkFileCautious filename

-- | Store pages of items by serializing them to NDJSON one item at a time. Each
-- item in the page is serialized as a separate item.
--
-- Note that this will not save information about how the items were originally
-- paged.
storePages ::
  (MonadResource m, MonoFoldable i, ToJSON (Element i)) =>
  FilePath ->
  ConduitM i o m ()
storePages filename =
  C.concatMap id
    .| store filename

-- | Use a newline-delimited JSON file as a source, emitting each line.
--
-- Throws 'ParseException' when the source file is unparseable.
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

-- | ParseExceptions are thrown when NDJSON files cannot be parsed when being
-- loaded.
data ParseException = ParseException
  { filename :: FilePath
  , errorMessage :: String
  }
  deriving (Show)

instance Exception ParseException where
  displayException (ParseException file msg) =
    "could not parse NDJSON at " <> show file <> ": " <> msg
