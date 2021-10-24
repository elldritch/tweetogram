-- | Download conduits of tweets.
module Tweetogram.Download (
  -- * Constructing clients
  DownloadOptions (..),
  Client,
  newClient,
  maxPageSize,

  -- * Loading tweets through Conduit
  sourceTimeline,
  sourceLikes,

  -- * Functions for loading tweets
  getStatuses,
  getPages,
  timelineR,
  likesR,
) where

import Conduit (PrimMonad)
import Control.Lens.Setter ((?~))
import Data.Conduit (ConduitT, (.|))
import Data.Conduit.Combinators (conduitVector)
import Data.Conduit.Combinators qualified as Conduit
import Data.Default (def)
import Data.Vector (Vector)
import Relude
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
import Web.Twitter.Types (Status (..))

-- | Options for constructing a 'Client'.
data DownloadOptions = DownloadOptions
  { twitterConsumerKey :: ByteString
  , twitterConsumerSecret :: ByteString
  , twitterAccessToken :: ByteString
  , twitterAccessTokenSecret :: ByteString
  , twitterUsername :: Text
  , pageSize :: Int
  }

-- | Clients are a single Twitter API client instance. Individual clients handle
-- their own HTTP connection multiplexing.
data Client = Client
  { connectionManager :: Manager
  , twInfo :: TWInfo
  , pageSize :: Int
  }

-- | Construct a new client instance.
newClient :: (MonadIO m) => DownloadOptions -> m Client
newClient DownloadOptions{..} = do
  connectionManager <- liftIO $ newManager tlsManagerSettings
  pure Client{connectionManager, twInfo, pageSize}
 where
  twInfo :: TWInfo
  twInfo =
    setCredential
      (twitterOAuth{oauthConsumerKey = twitterConsumerKey, oauthConsumerSecret = twitterConsumerSecret})
      (Credential [("oauth_token", twitterAccessToken), ("oauth_token_secret", twitterAccessTokenSecret)])
      def

-- | The maximum Twitter API response page size. Request the maximum page size
-- at a time to optimally consume rate limit, since the rate limit is applied on
-- API calls, not on returned tweets.
maxPageSize :: (Num a) => a
maxPageSize = 200

-- | Load a user's liked tweets as a conduit.
sourceLikes :: (MonadIO m, PrimMonad m) => Client -> UserParam -> ConduitT () Status m ()
sourceLikes client user = getStatuses client $ likesR user

-- | Load a user's posted timeline tweets as a conduit.
sourceTimeline :: (MonadIO m, PrimMonad m) => Client -> UserParam -> ConduitT () Status m ()
sourceTimeline client user = getStatuses client $ timelineR user

-- | Given an API request repeatable with @max_id@, load statuses as a conduit.
getStatuses ::
  ( MonadIO m
  , PrimMonad m
  , HasParam "max_id" Integer supports
  , HasParam "count" Integer supports
  , HasParam "tweet_mode" TweetMode supports
  ) =>
  Client ->
  APIRequest supports [Status] ->
  ConduitT () Status m ()
getStatuses client req =
  getPages client req
    .| Conduit.concatMap id

-- | Given an API request repeatable with @max_id@, load pages of statuses as a
-- conduit.
getPages ::
  ( MonadIO m
  , PrimMonad m
  , HasParam "max_id" Integer supports
  , HasParam "count" Integer supports
  , HasParam "tweet_mode" TweetMode supports
  ) =>
  Client ->
  APIRequest supports [Status] ->
  ConduitT () (Vector Status) m ()
getPages Client{..} req =
  sourceWithMaxId twInfo connectionManager req'
    .| conduitVector pageSize
 where
  req' =
    req
      & #count ?~ toInteger pageSize
      & #tweet_mode ?~ Extended

-- | Construct an API request for loading a user's timeline.
timelineR :: UserParam -> APIRequest StatusesUserTimeline [Status]
timelineR = (#include_rts ?~ True) . userTimeline

-- | Construct an API request for loading a user's liked tweets.
likesR :: UserParam -> APIRequest FavoritesList [Status]
likesR = favoritesList . Just
