module Main (main) where

import Conduit (PrimMonad, foldlC, mapMC)
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
import Data.Conduit.Throttle (
  Conf,
  newConf,
  setInterval,
  setMaxThroughput,
  throttleProducer,
 )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector (Vector)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Options.Applicative (
  Parser,
  ParserInfo,
  command,
  execParser,
  helper,
  hsubparser,
  info,
  long,
  progDesc,
  strOption,
 )
import Relude
import System.FilePath ((</>))
import Web.Twitter.Conduit (
  APIRequest,
  Credential (..),
  FavoritesList,
  Manager,
  OAuth (..),
  TWInfo (..),
  UserParam (..),
  def,
  favoritesList,
  newManager,
  setCredential,
  sourceWithMaxId,
  tlsManagerSettings,
  twitterOAuth,
 )
import Web.Twitter.Conduit.Parameters (TweetMode (..))
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
    <$> strOption (long "twitter-consumer-api-key")
    <*> strOption (long "twitter-consumer-api-key-secret")
    <*> strOption (long "twitter-access-token")
    <*> strOption (long "twitter-access-token-secret")
    <*> strOption (long "twitter-username")
    <*> strOption (long "data-dir")

newtype QueryOptions = QueryOptions
  { dataDir :: FilePath
  }

queryOptionsP :: Parser QueryOptions
queryOptionsP = QueryOptions <$> strOption (long "data-dir")

argsP :: ParserInfo Options
argsP = info (optionsP <**> helper) (progDesc "Compute statistics about your liked tweets")

newTWInfo :: DownloadOptions -> TWInfo
newTWInfo DownloadOptions{..} =
  setCredential
    (twitterOAuth{oauthConsumerKey = twitterConsumerKey, oauthConsumerSecret = twitterConsumerSecret})
    (Credential [("oauth_token", twitterAccessToken), ("oauth_token_secret", twitterAccessTokenSecret)])
    def

main :: IO ()
main = do
  Options{subcommand} <- execParser argsP
  case subcommand of
    Download downloadOptions -> download downloadOptions
    Query queryOptions -> query queryOptions

download :: DownloadOptions -> IO ()
download options@DownloadOptions{..} = do
  let twInfo = newTWInfo options
  connMgr <- newManager tlsManagerSettings

  putStrLn "Downloading liked tweets. This may take a few minutes due to Twitter's API rate limits."

  result <-
    try $
      runConduitRes $
        throttleProducer throttleConf (getLikes twInfo connMgr twitterUsername)
          .| concatMapE ((<> "\n") . toStrict . encode)
          .| void showProgress
          .| sinkFileCautious (dataDir </> "likes.ndjson")

  case result of
    Left err -> case fromException err of
      Just (IOError _ NoSuchThing _ description _ (Just filename)) ->
        putStrLn $ "Could not download liked tweets to " <> show filename <> ": " <> description
      Just _ -> putStrLn $ "Unexpected error: " <> displayException err
      Nothing -> putStrLn $ "Unexpected error: " <> displayException err
    Right () -> pure ()
 where
  pageSize :: (Num a) => a
  pageSize = 200

  -- We throttle on a per-page basis instead of a per-status basis because
  -- otherwise the throttle "smears" the 75*200 statuses I can request across
  -- the 15 minutes. This is slower overall than just throttling by page because
  -- statuses that have already been loaded still get throttled.
  throttleConf :: Conf a
  throttleConf =
    newConf
      & setMaxThroughput 75
      & setInterval (1000 * 60 * 15)

  getLikes :: (MonadIO m, PrimMonad m) => TWInfo -> Manager -> Text -> ConduitT () (Vector Status) m ()
  getLikes twInfo connMgr username =
    sourceWithMaxId twInfo connMgr (getLikesReq username)
      .| conduitVector pageSize

  getLikesReq :: Text -> APIRequest FavoritesList [Status]
  getLikesReq user =
    favoritesList
      (Just (ScreenNameParam $ toString user))
      & #count ?~ pageSize
      & #tweet_mode ?~ Extended

  showProgress :: (MonadIO m) => ConduitT i i m Int
  showProgress = (`mapAccumWhileM` 1) $ \x i -> do
    putStrLn $ "Downloaded page " <> show i <> " (" <> show (pageSize * i) <> " tweets total)."
    pure (Right (i + 1, x))

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
  , groupedLikes :: Map UserID (Set TweetID)
  }
  deriving (Show)

data LikedUser = LikedUser
  { userID :: Integer
  , screenName :: Text
  , name :: Text
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
    zero = Result{users = Map.empty, groupedLikes = Map.empty}

    f :: Result -> Status -> Result
    f Result{..} Status{statusId, statusUser = User{userId, userName, userScreenName}} =
      Result
        { users = Map.insert userId (LikedUser{userID = userId, screenName = userScreenName, name = userName}) users
        , groupedLikes = Map.insertWith Set.union userId (Set.singleton statusId) groupedLikes
        }

  render :: Result -> IO ()
  render Result{..} = case renderedLikes of
    Left err -> putStrLn err
    Right output -> putTextLn $ T.intercalate "\n" output
   where
    orderedLikes :: [(UserID, Set TweetID)]
    orderedLikes = sortOn (Down . Set.size . snd) $ Map.toList groupedLikes

    renderedLikes :: Either String [Text]
    renderedLikes = mapM renderLike orderedLikes

    renderLike :: (UserID, Set TweetID) -> Either String Text
    renderLike (userID, likedTweets) = case renderUser userID of
      Left s -> Left s
      Right t -> Right $ show (Set.size likedTweets) <> " - " <> t

    renderUser :: UserID -> Either String Text
    renderUser userID = case lookedUpUser of
      Just LikedUser{screenName} -> Right screenName
      Nothing -> Left $ "Impossible: inconsistent Tweetogram data: unknown user ID: " <> show userID
     where
      lookedUpUser = Map.lookup userID users
