module Main (main) where

import Relude

import Control.Exception (throwIO, try)
import Data.Aeson (eitherDecodeStrict')
import Data.Conduit (
  ConduitT,
  runConduitRes,
  (.|),
 )
import Data.Conduit.Combinators (
  sourceFile,
  splitOnUnboundedE,
 )
import Data.Conduit.Combinators qualified as C
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime (..), timeToTimeOfDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (
  TimeOfDay (..),
 )
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Options.Applicative.Extra (execParser)
import System.FilePath ((</>))
import Text.Layout.Table (asciiS, rowG, tableString, titlesH)
import Web.Twitter.Types (Status (..), User (..))

import Tweetogram.CLI.Download (download)
import Tweetogram.CLI.Options (
  Options (..),
  QueryActivityOptions (..),
  QueryLikesOptions (..),
  QuerySubcommand (..),
  Subcommand (..),
  TimeMode (..),
  argsP,
 )

main :: IO ()
main = do
  Options{subcommand} <- execParser argsP
  case subcommand of
    Download downloadOptions -> download downloadOptions
    Query queryCmd -> case queryCmd of
      QueryLikes queryLikesOptions -> queryLikes queryLikesOptions
      QueryActivity queryActivityOptions -> queryActivity queryActivityOptions

newtype ParseException = ParseException
  { errorMessage :: String
  }
  deriving (Show)

instance Exception ParseException where
  displayException (ParseException msg) = "could not parse Tweetogram data directory: " <> msg

type UserID = Integer

type TweetID = Integer

data LikesResult = LikesResult
  { users :: Map UserID LikedUser
  , tweets :: Map TweetID LikedTweet
  , groupedLikes :: Map UserID (Set TweetID)
  }
  deriving (Show)

-- TODO: Is being followed by target user?
--
-- NB: I can't use the "following" field for this, since that tells me whether
-- the _API user_ is following the author. Instead, I need to query
-- `GET friends/ids` and cross-reference against the author's user ID.

data LikedUser = LikedUser
  { userID :: Integer
  , screenName :: Text
  , displayName :: Text
  , isVerified :: Bool
  , createdAt :: UTCTime
  , followerCount :: Int
  , followingCount :: Int
  , tweetCount :: Int
  , likesCount :: Int
  }
  deriving (Show)

data LikedTweet = LikedTweet
  { tweetID :: Integer
  , -- Note: even though the official Twitter clients will display some accounts
    -- as "containing potentially sensitive content", this doesn't seem to be an
    -- actual field available on the API for users. Only individual _tweets_
    -- have a "potentially sensitive" field.
    --
    -- I'm not totally sure how the client decides to display this warning. My
    -- guess is that it takes a threshold percentage of potentially sensitive
    -- tweets, since it appears that some accounts that have sensitive tweets
    -- still don't show the warning.
    --
    -- See also:
    -- - v1 API user object model: https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/user
    -- - v2 API user object model: https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/user
    possiblySensitive :: Bool
  }
  deriving (Show)

queryLikes :: QueryLikesOptions -> IO ()
queryLikes QueryLikesOptions{..} = do
  result <-
    try $
      runConduitRes $
        sourceFile (dataDir </> "likes.ndjson")
          .| splitOnUnboundedE (== (toEnum $ ord '\n'))
          .| decodeLikes
          .| groupLikesByAuthor

  case result of
    Left err -> case fromException err of
      Just (IOError _ NoSuchThing _ description _ (Just filename)) ->
        putStrLn $ "Could not load liked tweets from " <> show filename <> ": " <> description
      Just _ -> putStrLn $ "Unexpected error: " <> displayException err
      Nothing -> putStrLn $ "Unexpected error: " <> displayException err
    Right r -> render r
 where
  decodeLikes :: (MonadIO m) => ConduitT ByteString Status m ()
  decodeLikes = C.mapM $ \line -> do
    case eitherDecodeStrict' line of
      Left err -> liftIO $ throwIO $ ParseException err
      Right status -> pure status

  groupLikesByAuthor :: (Monad m) => ConduitT Status o m LikesResult
  groupLikesByAuthor = C.foldl f zero
   where
    zero =
      LikesResult
        { users = Map.empty
        , tweets = Map.empty
        , groupedLikes = Map.empty
        }

    f :: LikesResult -> Status -> LikesResult
    f LikesResult{..} Status{statusUser = User{..}, ..} =
      LikesResult
        { users = Map.insert userId likedUser users
        , tweets = Map.insert statusId likedTweet tweets
        , groupedLikes = Map.insertWith Set.union userId (Set.singleton statusId) groupedLikes
        }
     where
      likedUser =
        LikedUser
          { userID = userId
          , screenName = userScreenName
          , displayName = userName
          , isVerified = userVerified
          , createdAt = userCreatedAt
          , followerCount = userFollowersCount
          , followingCount = userFriendsCount
          , tweetCount = userStatusesCount
          , likesCount = userFavoritesCount
          }

      -- TODO: I should just roll this directly into the User field instead of
      -- saving a list of all tweets. Use a Users Map update on sensitive
      -- tweets. Add a User field called "has sensitive tweets".
      likedTweet =
        LikedTweet
          { tweetID = statusId
          , possiblySensitive = Just True == statusPossiblySensitive
          }

  render :: LikesResult -> IO ()
  render LikesResult{..} =
    putStrLn $
      tableString
        (fmap (const def) headers)
        asciiS
        (titlesH headers)
        $ fmap rowG rows
   where
    ordered :: [(LikedUser, Set TweetID)]
    ordered =
      sortOn (Down . Set.size . snd) $
        sortOn (screenName . fst) $
          first getUser <$> Map.toList groupedLikes

    getUser :: UserID -> LikedUser
    getUser userID = case Map.lookup userID users of
      Just lu -> lu
      Nothing -> error $ "impossible: inconsistent Tweetogram data: unknown user ID: " <> show userID

    filtered :: [(LikedUser, Set TweetID)]
    filtered = filterTopN $ filterMinLikes ordered
     where
      filterMinLikes :: [(LikedUser, Set TweetID)] -> [(LikedUser, Set TweetID)]
      filterMinLikes = case minLikes of
        Just n -> filter ((>= n) . Set.size . snd)
        Nothing -> id

      filterTopN :: [(LikedUser, Set TweetID)] -> [(LikedUser, Set TweetID)]
      filterTopN = maybe id take topN

    hydrated :: [(LikedUser, [LikedTweet])]
    hydrated = second (fmap getTweet . toList) <$> filtered

    getTweet :: TweetID -> LikedTweet
    getTweet tweetID = case Map.lookup tweetID tweets of
      Just lt -> lt
      Nothing -> error $ "impossible: inconsistent Tweetogram data: unknown tweet ID: " <> show tweetID

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
    rows = f <$> zip [0 ..] hydrated
     where
      f :: (Integer, (LikedUser, [LikedTweet])) -> [String]
      f (i, (LikedUser{..}, likes)) =
        [ show (i + 1)
        , show (length likes)
        , toString screenName
        , toString displayName
        , show isVerified
        , show (any possiblySensitive likes)
        , show followerCount
        , show followingCount
        , show tweetCount
        , show likesCount
        , show createdAt
        ]

type Hour = Int

type TweetCount = Int

-- TODO: This should probably get broken up into modules.
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
  decodeTweets = C.mapM $ \line -> do
    case eitherDecodeStrict' line of
      Left err -> liftIO $ throwIO $ ParseException err
      Right status -> pure status

  groupTweetsByTime :: (Monad m) => ConduitT Status o m (Map Hour TweetCount)
  groupTweetsByTime = C.foldl f $ Map.fromList $ zip [0 .. 23] $ repeat 0
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
