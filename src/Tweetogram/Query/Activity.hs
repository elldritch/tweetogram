module Tweetogram.Query.Activity () where

import Conduit (foldlC, mapMC)
import Control.Exception (throwIO, try)
import Data.Aeson (eitherDecodeStrict')
import Data.Conduit (ConduitT, runConduitRes, (.|))
import Data.Conduit.Combinators (sourceFile, splitOnUnboundedE)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Time (TimeOfDay (..), UTCTime (..), defaultTimeLocale, formatTime, timeToTimeOfDay)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.Show qualified (Show (..))
import Relude
import System.FilePath ((</>))
import Text.Layout.Table (asciiS, rowG, tableString, titlesH)
import Web.Twitter.Types (Status (..))

data QueryActivityOptions = QueryActivityOptions
  { dataDir :: FilePath
  , -- TODO:
    -- - Support "+N" positive offsets.
    -- - Support "+H:MM" offsets e.g. "-9:30" for French Polynesia.
    -- - Support timezone-by-name, with lookup of corresponding offset?
    tzOffset :: Maybe Int
  , timeMode :: TimeMode
  }

data TimeMode = Display24H | Display12H

instance Show TimeMode where
  show Display12H = "12h"
  show Display24H = "24h"

type Hour = Int

type TweetCount = Int

newtype ParseException = ParseException
  { errorMessage :: String
  }
  deriving (Show)

instance Exception ParseException where
  displayException (ParseException msg) = "could not parse Tweetogram data directory: " <> msg

queryActivity :: QueryActivityOptions -> IO ()
queryActivity QueryActivityOptions{..} = do
  result <-
    try $
      runConduitRes $
        sourceFile (dataDir </> "timeline.ndjson")
          .| splitOnUnboundedE (== (toEnum $ ord '\n'))
          .| decodeTweets
          .| groupTweetsByTime

  case result of
    Left err -> case fromException err of
      Just (IOError _ NoSuchThing _ description _ (Just filename)) ->
        putStrLn $ "Could not load liked tweets from " <> show filename <> ": " <> description
      Just _ -> putStrLn $ "Unexpected error: " <> displayException err
      Nothing -> putStrLn $ "Unexpected error: " <> displayException err
    Right r -> render r
 where
  decodeTweets :: (MonadIO m) => ConduitT ByteString Status m ()
  decodeTweets = mapMC $ \line -> do
    case eitherDecodeStrict' line of
      Left err -> liftIO $ throwIO $ ParseException err
      Right status -> pure status

  groupTweetsByTime :: (Monad m) => ConduitT Status o m (Map Hour TweetCount)
  groupTweetsByTime = foldlC f $ Map.fromList $ zip [0 .. 23] $ repeat 0
   where
    f :: Map Hour TweetCount -> Status -> Map Hour TweetCount
    f m Status{statusCreatedAt = UTCTime{utctDayTime = dayTime}} =
      Map.insertWith (+) hour 1 m
     where
      TimeOfDay{todHour = hour} = timeToTimeOfDay dayTime

  render :: Map Hour TweetCount -> IO ()
  render m =
    putStrLn $
      tableString
        (fmap (const def) headers)
        asciiS
        (titlesH headers)
        $ fmap rowG rows
   where
    headers :: [String]
    headers =
      ["Time (UTC)"]
        ++ ( case tzOffset of
              Just x | x >= 0 -> ["Time (UTC+" <> show x <> ")"]
              Just x -> ["Time (UTC" <> show x <> ")"]
              Nothing -> []
           )
        ++ [ "Count"
           , "Bar"
           ]

    rows :: [[String]]
    rows =
      ( \(h, c) ->
          [fmtHour h]
            ++ maybe [] (\tzo -> [fmtHour $ h + tzo]) tzOffset
            ++ [ show c
               , replicate (c * 30 `div` maxCount) 'X'
               ]
      )
        <$> Map.toAscList m

    maxCount = Map.foldr max 0 m

    fmtHour h = case timeMode of
      Display24H -> formatTime defaultTimeLocale "%R" (TimeOfDay{todHour = (h + 2) `mod` 24, todMin = 0, todSec = 0})
      Display12H -> formatTime defaultTimeLocale "%l %p" (TimeOfDay{todHour = (h + 2) `mod` 24, todMin = 0, todSec = 0})
