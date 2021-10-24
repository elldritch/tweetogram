module Tweetogram.Download () where

import Conduit (PrimMonad)
import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, try)
import Control.Lens.Setter ((?~))
import Data.Aeson (encode)
import Data.Conduit (ConduitT, runConduitRes, (.|))
import Data.Conduit.Combinators (
  concatMapE,
  conduitVector,
  mapAccumWhileM,
  sinkFileCautious,
 )
import Data.Default (def)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (
  TimeZone,
  getCurrentTimeZone,
  utcToLocalTime,
 )
import Data.Vector (Vector)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Relude
import System.Console.Concurrent (outputConcurrent, withConcurrentOutput)
import System.FilePath ((</>))
import Web.Twitter.Conduit (
  APIRequest,
  Credential (..),
  HasParam,
  Manager,
  OAuth (..),
  TWInfo,
  TwitterError (..),
  TwitterErrorMessage (..),
  newManager,
  setCredential,
  sourceWithMaxId,
  tlsManagerSettings,
  twitterOAuth,
 )
import Web.Twitter.Conduit.Api (FavoritesList, favoritesList)
import Web.Twitter.Conduit.Parameters (TweetMode (..), UserParam (..))
import Web.Twitter.Conduit.Status (StatusesUserTimeline, userTimeline)
import Web.Twitter.Types (Status (..))

data DownloadOptions = DownloadOptions
  { twitterConsumerKey :: ByteString
  , twitterConsumerSecret :: ByteString
  , twitterAccessToken :: ByteString
  , twitterAccessTokenSecret :: ByteString
  , twitterUsername :: Text
  , dataDir :: FilePath
  }

download :: DownloadOptions -> IO ()
download DownloadOptions{..} = do
  connMgr <- newManager tlsManagerSettings
  withConcurrentOutput $ do
    result <-
      try $
        void $
          concurrently
            (runPipeline connMgr "Downloading user timeline" (dataDir </> "timeline.ndjson") $ timelineReq twitterUsername)
            (runPipeline connMgr "Downloading liked tweets" (dataDir </> "likes.ndjson") $ likesReq twitterUsername)
    handleError result
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
    Manager ->
    String ->
    FilePath ->
    APIRequest supports [Status] ->
    IO ()
  runPipeline connMgr progress filename req =
    runConduitRes $
      getPages connMgr req
        .| concatMapE ((<> "\n") . toStrict . encode)
        .| void (showProgress progress)
        .| sinkFileCautious filename

  getPages ::
    ( MonadIO m
    , PrimMonad m
    , HasParam "max_id" Integer supports
    , HasParam "count" Integer supports
    , HasParam "tweet_mode" TweetMode supports
    ) =>
    Manager ->
    APIRequest supports [Status] ->
    ConduitT () (Vector Status) m ()
  getPages connMgr req =
    sourceWithMaxId twInfo connMgr req'
      .| conduitVector pageSize
   where
    req' =
      req
        & #count ?~ pageSize
        & #tweet_mode ?~ Extended

  timelineReq :: Text -> APIRequest StatusesUserTimeline [Status]
  timelineReq = (#include_rts ?~ True) . userTimeline . ScreenNameParam . toString

  likesReq :: Text -> APIRequest FavoritesList [Status]
  likesReq = favoritesList . Just . ScreenNameParam . toString

  showProgress :: (MonadIO m) => String -> ConduitT i i m Int
  showProgress name = (`mapAccumWhileM` 1) $ \x i -> do
    liftIO $ outputConcurrent $ name <> ": downloaded page " <> show i <> " (" <> show (pageSize * i) <> " tweets total).\n"
    pure (Right (i + 1, x))

  handleError :: Either SomeException a -> IO a
  handleError result = do
    tz <- getCurrentTimeZone
    case result of
      Right r -> pure r
      Left err -> die $ fromMaybe ("Unexpected error: " <> displayException err) $ fmtErrs tz err
   where
    fmtErrs :: TimeZone -> SomeException -> Maybe String
    fmtErrs tz err =
      (fromException err >>= fmtIOErr)
        <|> (fromException err >>= fmtTwitterErr tz)

    fmtIOErr :: IOException -> Maybe String
    fmtIOErr (IOError _ NoSuchThing _ description _ (Just filename)) =
      Just $ "Could not download tweets to " <> show filename <> ": " <> description
    fmtIOErr _ = Nothing

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
