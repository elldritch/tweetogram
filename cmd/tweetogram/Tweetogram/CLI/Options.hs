-- | Argument parsing for Tweetogram.
module Tweetogram.CLI.Options (
  -- * Argument parser
  argsP,

  -- * Options parsers
  Options (..),
  optionsP,
  Subcommand (..),
  subcommandP,
  QuerySubcommand (..),
  querySubcommandP,
) where

import Relude

import Options.Applicative.Builder (command, info, progDesc)
import Options.Applicative.Extra (helper, hsubparser)
import Options.Applicative.Types (Parser, ParserInfo)

import Tweetogram.CLI.Download (DownloadOptions, downloadOptionsP)
import Tweetogram.CLI.Query.Activity (
  QueryActivityOptions,
  queryActivityOptionsP,
 )
import Tweetogram.CLI.Query.Likes (QueryLikesOptions, queryLikesOptionsP)

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

data QuerySubcommand
  = QueryLikes QueryLikesOptions
  | QueryActivity QueryActivityOptions

querySubcommandP :: Parser QuerySubcommand
querySubcommandP = hsubparser (likesC <> activityC)
 where
  likesC = command "likes" (info (QueryLikes <$> queryLikesOptionsP) $ progDesc "Show liked tweets")
  activityC = command "activity" (info (QueryActivity <$> queryActivityOptionsP) $ progDesc "Show tweet activity")

argsP :: ParserInfo Options
argsP = info (optionsP <**> helper) (progDesc "Compute statistics about your tweets")
