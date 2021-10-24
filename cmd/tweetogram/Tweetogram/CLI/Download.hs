module Tweetogram.CLI.Download (
  -- * Command line options
  DownloadOptions (..),
  downloadOptionsP,

  -- * Command implementation
  download,
) where

import Relude

import Control.Concurrent.Async (concurrently)
import Control.Exception (try)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit (
  ConduitT,
  runConduitRes,
  (.|),
 )
import Data.Conduit.Combinators (
  mapAccumWhileM,
 )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (
  TimeZone,
  getCurrentTimeZone,
  utcToLocalTime,
 )
import Options.Applicative (Parser, help, long, strOption)
import System.Console.Concurrent (outputConcurrent, withConcurrentOutput)
import System.FilePath ((</>))
import Web.Twitter.Conduit (
  TwitterError (..),
  TwitterErrorMessage (..),
 )
import Web.Twitter.Conduit.Parameters (UserParam (..))
import Web.Twitter.Types (Status (..))

import Tweetogram.CLI.Errors (showExceptions)
import Tweetogram.CLI.Store (fmtStoreErr)
import Tweetogram.Download (
  Client,
  Config (..),
  newClient,
  sourceLikes,
  sourceTimeline,
 )
import Tweetogram.Store (store)

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

type TweetSource = ConduitT () Status (ResourceT IO) ()

type Pipeline = ConduitT () Void (ResourceT IO) ()

type PageCount = Int

download :: DownloadOptions -> IO ()
download DownloadOptions{..} = do
  client <- newClient Config{..}
  let pipe = pipeline client

  withConcurrentOutput $ do
    result <-
      try $
        void $
          concurrently
            (runConduitRes $ pipe sourceTimeline "timeline.ndjson" $ showProgress "Downloading user timeline")
            (runConduitRes $ pipe sourceLikes "likes.ndjson" $ showProgress "Downloading liked tweets")
    showExceptions result $ \err -> do
      tz <- getCurrentTimeZone
      pure $
        (fromException err >>= fmtStoreErr)
          <|> (fromException err >>= fmtTwitterErr tz)
 where
  pageSize :: Int
  pageSize = 200

  pipeline ::
    Client ->
    (Client -> UserParam -> TweetSource) ->
    FilePath ->
    (PageCount -> IO ()) ->
    Pipeline
  pipeline client source filename progress =
    source client (ScreenNameParam $ toString twitterUsername)
      .| void (onProgress progress)
      .| store (dataDir </> filename)

  showProgress :: Text -> PageCount -> IO ()
  showProgress name i =
    if i `mod` 200 == 0
      then
        outputConcurrent $
          name <> ": downloaded page " <> show (i `div` 200) <> " (" <> show i <> " tweets total).\n"
      else pure ()

  onProgress :: (MonadIO m) => (PageCount -> IO ()) -> ConduitT i i m Int
  onProgress f =
    (`mapAccumWhileM` 1) $ \x i -> do
      liftIO $ f i
      pure (Right (i + 1, x))

  fmtTwitterErr :: TimeZone -> TwitterError -> Maybe String
  fmtTwitterErr tz (TwitterErrorResponse _ headers [TwitterErrorMessage 88 _]) =
    Just $
      fromMaybe "Error: Twitter API rate limit reached" $ do
        (_, resetTime) <- find ((== "x-rate-limit-reset") . fst) headers
        timestamp <- readMaybe $ decodeUtf8 resetTime
        pure $
          "Error: Twitter API rate limit reached (rate limit resets at "
            <> show (utcToLocalTime tz $ posixSecondsToUTCTime $ fromInteger timestamp)
            <> " "
            <> show tz
            <> ")"
  fmtTwitterErr _ _ = Nothing
