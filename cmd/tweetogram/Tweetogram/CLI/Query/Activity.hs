module Tweetogram.CLI.Query.Activity (
  -- * Command line options
  QueryActivityOptions (..),
  queryActivityOptionsP,

  -- * New types
  TimeMode (..),
  readTimeMode,

  -- * Command implementation
  queryActivity,
) where

import Relude

import GHC.Show qualified (Show (..))
import Options.Applicative.Builder (
  ReadM,
  auto,
  eitherReader,
  help,
  long,
  metavar,
  option,
  showDefault,
  strOption,
  value,
 )
import Options.Applicative.Types (Parser)

import Control.Exception (try)
import Data.Conduit (
  runConduitRes,
  (.|),
 )
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (
  TimeOfDay (..),
 )
import System.FilePath ((</>))
import Text.Layout.Table (asciiS, rowG, tableString, titlesH)

import Tweetogram.CLI.Errors (showExceptions)
import Tweetogram.CLI.Store (fmtLoadErr)
import Tweetogram.Query.Activity (Hour, tweetActivity)
import Tweetogram.Store (load)

data TimeMode = Display24H | Display12H

instance Show TimeMode where
  show Display12H = "12h"
  show Display24H = "24h"

readTimeMode :: ReadM TimeMode
readTimeMode = eitherReader $ \case
  "24h" -> Right Display24H
  "12h" -> Right Display12H
  _ -> Left "could not read time mode: must be either \"24h\" or \"12h\""

data QueryActivityOptions = QueryActivityOptions
  { dataDir :: FilePath
  , -- TODO:
    -- - Support non-integer offsets.
    -- - Support "+N" positive offsets.
    -- - Support "+H:MM" offsets e.g. "-9:30" for French Polynesia.
    -- - Support timezone-by-name, with lookup of corresponding offset?
    tzOffset :: Maybe Int
  , timeMode :: TimeMode
  }

queryActivityOptionsP :: Parser QueryActivityOptions
queryActivityOptionsP =
  QueryActivityOptions
    <$> strOption (long "data-dir" <> help "Filepath to a directory containing downloaded tweets")
    <*> optional (option auto (long "tz-offset" <> help "Timezone offset (+/-N from GMT) to check" <> metavar "+/-N"))
    <*> option readTimeMode (long "time-mode" <> help "Time display mode (either \"24h\" or \"12h\")" <> value Display24H <> showDefault)

queryActivity :: QueryActivityOptions -> IO ()
queryActivity QueryActivityOptions{..} = do
  result <-
    try $
      runConduitRes $
        load (dataDir </> "timeline.ndjson")
          .| tweetActivity

  success <- showExceptions result $ \err -> do
    pure $ fromException err >>= fmtLoadErr

  render success
 where
  render :: Map Hour Int -> IO ()
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
