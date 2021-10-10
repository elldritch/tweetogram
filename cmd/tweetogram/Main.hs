module Main (main) where

import Conduit (PrimMonad, foldlC, mapMC)
import Control.Concurrent.Async (concurrently)
import Control.Exception (throwIO, try)
import Control.Lens.Setter ((?~))
import Data.Aeson (eitherDecodeStrict', encode)
import Data.Conduit (
  ConduitT,
  runConduitRes,
  (.|),
 )
import Data.Conduit.Combinators (
  concatMapE,
  conduitVector,
  mapAccumWhileM,
  sinkFileCautious,
  sourceFile,
  splitOnUnboundedE,
 )
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Data.Vector (Vector)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
  execParser,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  option,
  progDesc,
  strOption,
 )
import Relude
import System.Console.Concurrent (outputConcurrent, withConcurrentOutput)
import System.FilePath ((</>))
import Text.Layout.Table (asciiS, rowG, tableString, titlesH)
import Web.Twitter.Conduit (
  APIRequest,
  Credential (..),
  HasParam,
  Manager,
  OAuth (..),
  TWInfo,
  newManager,
  setCredential,
  sourceWithMaxId,
  tlsManagerSettings,
  twitterOAuth,
 )
import Web.Twitter.Conduit.Api (FavoritesList, favoritesList)
import Web.Twitter.Conduit.Parameters (TweetMode (..), UserParam (..))
import Web.Twitter.Conduit.Status (StatusesUserTimeline, userTimeline)
import Web.Twitter.Types (Status (..), User (..))

newtype Options = Options
  { subcommand :: Subcommand
  }

data Subcommand
  = Download DownloadOptions
  | Query QueryOptions

subcommandP :: Parser Subcommand
subcommandP = hsubparser (downloadC <> queryC)
 where
  downloadC = command "download" (info (Download <$> downloadOptionsP) $ progDesc "Download your liked tweets")
  queryC = command "query" (info (Query <$> queryOptionsP) $ progDesc "Query statistics about liked tweets")

optionsP :: Parser Options
optionsP = Options <$> subcommandP

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

-- TODO:
-- - Sort on different columns
-- - Filter on different columns

data QueryOptions = QueryOptions
  { dataDir :: FilePath
  , topN :: Maybe Int
  , minLikes :: Maybe Int
  }

queryOptionsP :: Parser QueryOptions
queryOptionsP =
  QueryOptions
    <$> strOption (long "data-dir" <> help "Filepath to a directory containing downloaded tweets")
    <*> optional (option auto (long "top" <> help "Only show top N most liked accounts" <> metavar "N"))
    <*> optional (option auto (long "min-likes" <> help "Only show accounts with at least N likes" <> metavar "N"))

argsP :: ParserInfo Options
argsP = info (optionsP <**> helper) (progDesc "Compute statistics about your liked tweets")

main :: IO ()
main = do
  Options{subcommand} <- execParser argsP
  case subcommand of
    Download downloadOptions -> download downloadOptions
    Query queryOptions -> query queryOptions

download :: DownloadOptions -> IO ()
download DownloadOptions{..} = do
  connMgr <- newManager tlsManagerSettings

  putStrLn "Downloading tweets. This may take a few minutes due to Twitter's API rate limits."

  withConcurrentOutput $ do
    result <-
      try $
        void $
          concurrently
            (runPipeline "Downloading user timeline" (dataDir </> "timeline.ndjson") connMgr timelineReq twitterUsername)
            (runPipeline "Downloading liked tweets" (dataDir </> "likes.ndjson") connMgr likesReq twitterUsername)
    handleIOError result
 where
  -- Request the maximum page size at a time to optimally consume rate limit.
  pageSize :: (Num a) => a
  pageSize = 200

  twInfo :: TWInfo
  twInfo =
    setCredential
      (twitterOAuth{oauthConsumerKey = twitterConsumerKey, oauthConsumerSecret = twitterConsumerSecret})
      (Credential [("oauth_token", twitterAccessToken), ("oauth_token_secret", twitterAccessTokenSecret)])
      def

  runPipeline ::
    ( HasParam "max_id" Integer supports
    , HasParam "count" Integer supports
    , HasParam "tweet_mode" TweetMode supports
    ) =>
    String ->
    FilePath ->
    Manager ->
    (Text -> APIRequest supports [Status]) ->
    Text ->
    IO ()
  runPipeline progress filename connMgr req user =
    runConduitRes $
      getPages connMgr req user
        .| concatMapE ((<> "\n") . toStrict . encode)
        .| void (showProgress progress)
        .| sinkFileCautious filename

  getPages ::
    forall m supports.
    ( MonadIO m
    , PrimMonad m
    , HasParam "max_id" Integer supports
    , HasParam "count" Integer supports
    , HasParam "tweet_mode" TweetMode supports
    ) =>
    Manager ->
    (Text -> APIRequest supports [Status]) ->
    Text ->
    ConduitT () (Vector Status) m ()
  getPages connMgr makeReq username =
    sourceWithMaxId twInfo connMgr req
      .| conduitVector pageSize
   where
    req :: APIRequest supports [Status]
    req =
      makeReq username
        & #count ?~ pageSize
        & #tweet_mode ?~ Extended

  timelineReq :: Text -> APIRequest StatusesUserTimeline [Status]
  timelineReq = userTimeline . ScreenNameParam . toString

  likesReq :: Text -> APIRequest FavoritesList [Status]
  likesReq = favoritesList . Just . ScreenNameParam . toString

  showProgress :: (MonadIO m) => String -> ConduitT i i m Int
  showProgress name = (`mapAccumWhileM` 1) $ \x i -> do
    liftIO $ outputConcurrent $ name <> ": downloaded page " <> show i <> " (" <> show (pageSize * i) <> " tweets total).\n"
    pure (Right (i + 1, x))

  handleIOError :: Either SomeException a -> IO a
  handleIOError result = case result of
    Right r -> pure r
    Left err -> die $ fmt err
   where
    fmt err = case fromException err of
      Just (IOError _ NoSuchThing _ description _ (Just filename)) ->
        "Could not download tweets to " <> show filename <> ": " <> description
      _ -> "Unexpected error: " <> displayException err

newtype ParseException = ParseException
  { errorMessage :: String
  }
  deriving (Show)

instance Exception ParseException where
  displayException (ParseException msg) = "could not parse Tweetogram data directory: " <> msg

type UserID = Integer

type TweetID = Integer

data Result = Result
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

query :: QueryOptions -> IO ()
query QueryOptions{..} = do
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
  decodeLikes = mapMC $ \line -> do
    case eitherDecodeStrict' line of
      Left err -> liftIO $ throwIO $ ParseException err
      Right status -> pure status

  groupLikesByAuthor :: (Monad m) => ConduitT Status o m Result
  groupLikesByAuthor = foldlC f zero
   where
    zero =
      Result
        { users = Map.empty
        , tweets = Map.empty
        , groupedLikes = Map.empty
        }

    f :: Result -> Status -> Result
    f Result{..} Status{statusUser = User{..}, ..} =
      Result
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

      likedTweet =
        LikedTweet
          { tweetID = statusId
          , possiblySensitive = fromMaybe False statusPossiblySensitive
          }

  render :: Result -> IO ()
  render Result{..} =
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
