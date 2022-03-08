module Tweetogram.Shared.User (
  UserWith(..),
  UserID,
) where

import Relude

import Data.Time.Clock (UTCTime)

-- | A Twitter user's ID.
type UserID = Integer

-- TODO: Field idea: "is being followed by target user?"
--
-- NB: I can't use the "following" field for this, since that tells me whether
-- the _API user_ is following the author. Instead, I need to query
-- `GET friends/ids` and cross-reference against the author's user ID.

-- | A Twitter user whose tweet has been liked.
--
-- Note that the fields in this type are for the author user themselves. For
-- example, 'likesCount' is the count of tweets made by this user, not the count
-- of tweets of this user that have been liked by the querying user.
data UserWith a = TwitterUser
  { screenName :: Text
  , displayName :: Text
  , isVerified :: Bool
  , createdAt :: UTCTime
  , followerCount :: Int
  , followingCount :: Int
  , tweetCount :: Int
  , likesCount :: Int
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
    --
    -- This field is set to true if any tweet from this user has its
    -- "potentially sensitive tweet" field set to true.
    hasPossiblySensitiveTweets :: Bool
  , metadata :: a
  }
  deriving (Show)
