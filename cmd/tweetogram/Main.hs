module Main (main) where

import Relude

import Options.Applicative.Extra (execParser)

import Tweetogram.CLI.Download (download)
import Tweetogram.CLI.Options (
  Options (..),
  QuerySubcommand (..),
  Subcommand (..),
  argsP,
 )
import Tweetogram.CLI.Query.Activity (queryActivity)
import Tweetogram.CLI.Query.Likes (queryLikes)

main :: IO ()
main = do
  Options{subcommand} <- execParser argsP
  case subcommand of
    Download downloadOptions -> download downloadOptions
    Query queryCmd -> case queryCmd of
      QueryLikes queryLikesOptions -> queryLikes queryLikesOptions
      QueryActivity queryActivityOptions -> queryActivity queryActivityOptions
