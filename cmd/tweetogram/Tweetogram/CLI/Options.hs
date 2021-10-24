-- | Argument parsing for Tweetogram.
module Tweetogram.CLI.Options (
  -- * Argument parser
  argsP,

  -- * Options parsers
  Options (..),
  optionsP,
  Subcommand (..),
  subcommandP,
  DownloadOptions (..),
  downloadOptionsP,
  QuerySubcommand (..),
  querySubcommandP,
  QueryLikesOptions (..),
  queryLikesOptionsP,
  QueryActivityOptions (..),
  queryActivityOptionsP,

  -- * New types
  TimeMode (..),
) where

import GHC.Show qualified (Show (..))

import Options.Applicative.Builder (
  ReadM,
  auto,
  command,
  eitherReader,
  help,
  info,
  long,
  metavar,
  option,
  progDesc,
  showDefault,
  strOption,
  value,
 )
import Options.Applicative.Extra (helper, hsubparser)
import Options.Applicative.Types (Parser, ParserInfo)

import Relude

newtype Options = Options
  { subcommand :: Subcommand
  }

optionsP :: Parser Options
optionsP = Options <$> subcommandP

data Subcommand
  = Download DownloadOptions
  | Query QuerySubcommand

subcommandP :: Parser Subcommand
subcommandP = hsubparser (downloadC <> queryC)
 where
  downloadC = command "download" (info (Download <$> downloadOptionsP) $ progDesc "Download your tweets")
  queryC = command "query" (info (Query <$> querySubcommandP) $ progDesc "Show statistics about your tweets")

data DownloadOptions = DownloadOptions
  { twitterConsumerKey :: ByteString
  , twitterConsumerSecret :: ByteString
  , twitterAccessToken :: ByteString
  , twitterAccessTokenSecret :: ByteString
  , twitterUsername :: Text
  , dataDir :: FilePath
  }

downloadOptionsP :: Parser DownloadOptions
downloadOptionsP =
  DownloadOptions
    <$> strOption (long "twitter-consumer-api-key" <> help "Your \"Consumer Keys: API Key\" from the Twitter Developer Portal")
    <*> strOption (long "twitter-consumer-api-key-secret" <> help "Your \"Consumer Keys: API Key Secret\" from the Twitter Developer Portal")
    <*> strOption (long "twitter-access-token" <> help "Your \"Authentication Tokens: Access Token\" from the Twitter Developer Portal")
    <*> strOption (long "twitter-access-token-secret" <> help "Your \"Authentication Tokens: Access Token Secret\" from the Twitter Developer Portal")
    <*> strOption (long "twitter-username" <> help "Username of the account to download liked tweets from")
    <*> strOption (long "data-dir" <> help "Filepath to a directory to save downloaded tweets")

data QuerySubcommand
  = QueryLikes QueryLikesOptions
  | QueryActivity QueryActivityOptions

querySubcommandP :: Parser QuerySubcommand
querySubcommandP = hsubparser (likesC <> activityC)
 where
  likesC = command "likes" (info (QueryLikes <$> queryLikesOptionsP) $ progDesc "Show liked tweets")
  activityC = command "activity" (info (QueryActivity <$> queryActivityOptionsP) $ progDesc "Show tweet activity")

-- TODO:
-- - Sort on different columns
-- - Filter on different columns

data QueryLikesOptions = QueryLikesOptions
  { dataDir :: FilePath
  , topN :: Maybe Int
  , minLikes :: Maybe Int
  }

queryLikesOptionsP :: Parser QueryLikesOptions
queryLikesOptionsP =
  QueryLikesOptions
    <$> strOption (long "data-dir" <> help "Filepath to a directory containing downloaded tweets")
    <*> optional (option auto (long "top" <> help "Only show top N most liked accounts" <> metavar "N"))
    <*> optional (option auto (long "min-likes" <> help "Only show accounts with at least N likes" <> metavar "N"))

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

data TimeMode = Display24H | Display12H

instance Show TimeMode where
  show Display12H = "12h"
  show Display24H = "24h"

readTimeMode :: ReadM TimeMode
readTimeMode = eitherReader $ \case
  "24h" -> Right Display24H
  "12h" -> Right Display12H
  _ -> Left "could not read time mode: must be either \"24h\" or \"12h\""

queryActivityOptionsP :: Parser QueryActivityOptions
queryActivityOptionsP =
  QueryActivityOptions
    <$> strOption (long "data-dir" <> help "Filepath to a directory containing downloaded tweets")
    <*> optional (option auto (long "tz-offset" <> help "Timezone offset (+/-N from GMT) to check" <> metavar "+/-N"))
    <*> option readTimeMode (long "time-mode" <> help "Time display mode (either \"24h\" or \"12h\")" <> value Display24H <> showDefault)

argsP :: ParserInfo Options
argsP = info (optionsP <**> helper) (progDesc "Compute statistics about your tweets")
