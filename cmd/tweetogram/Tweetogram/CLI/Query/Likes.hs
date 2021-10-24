module Tweetogram.CLI.Query.Likes (
  -- * Command line options
  QueryLikesOptions (..),
  queryLikesOptionsP,

  -- * Command implementation
  queryLikes,
) where

import Relude

import Control.Exception (try)
import Data.Conduit (runConduitRes, (.|))
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Options.Applicative (Parser)
import Options.Applicative.Builder (
  auto,
  help,
  long,
  metavar,
  option,
  strOption,
 )
import System.FilePath ((</>))
import Text.Layout.Table (asciiS, rowG, tableString, titlesH)

import Tweetogram.CLI.Errors (showExceptions)
import Tweetogram.CLI.Store (fmtLoadErr)
import Tweetogram.Query.Likes (Liked (..), LikedUser (..), UserID, likedAuthors)
import Tweetogram.Store (load)

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

queryLikes :: QueryLikesOptions -> IO ()
queryLikes QueryLikesOptions{..} = do
  result <-
    try $
      runConduitRes $
        load (dataDir </> "likes.ndjson")
          .| likedAuthors

  success <- showExceptions result $ \err -> do
    pure $ fromException err >>= fmtLoadErr

  render success
 where
  render :: Liked -> IO ()
  render Liked{..} =
    putStrLn $
      tableString
        (fmap (const def) headers)
        asciiS
        (titlesH headers)
        $ fmap rowG rows
   where
    ordered :: [(LikedUser, Int)]
    ordered =
      sortOn (Down . snd) $
        sortOn (screenName . fst) $
          first getUser <$> Map.toList tweetsByUser

    getUser :: UserID -> LikedUser
    getUser userID = case Map.lookup userID users of
      Just lu -> lu
      Nothing -> error $ "impossible: inconsistent Tweetogram data: unknown user ID: " <> show userID

    filtered :: [(LikedUser, Int)]
    filtered = filterTopN $ filterMinLikes ordered
     where
      filterMinLikes :: [(LikedUser, Int)] -> [(LikedUser, Int)]
      filterMinLikes = case minLikes of
        Just n -> filter ((>= n) . snd)
        Nothing -> id

      filterTopN :: [(LikedUser, Int)] -> [(LikedUser, Int)]
      filterTopN = maybe id take topN

    headers :: [String]
    headers =
      [ "Rank"
      , "Liked tweets"
      , "Handle"
      , "Name"
      , "Verified?"
      , "NSFW*?"
      , "Followers"
      , "Following"
      , "Tweets"
      , "Likes"
      , "Created"
      ]

    rows :: [[String]]
    rows = f <$> zip [0 ..] filtered
     where
      f :: (Integer, (LikedUser, Int)) -> [String]
      f (i, (LikedUser{..}, likedCount)) =
        [ show (i + 1)
        , show likedCount
        , toString screenName
        , toString displayName
        , show isVerified
        , show hasPossiblySensitiveTweets
        , show followerCount
        , show followingCount
        , show tweetCount
        , show likesCount
        , show createdAt
        ]
