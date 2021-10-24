-- | Analyze whose tweets a user has liked.
module Tweetogram.Query.Likes (
  LikedUser (..),
  Liked (..),
  UserID,
  likedAuthors,
) where

import Relude

import Data.Conduit (ConduitT)
import Data.Conduit.Combinators qualified as C
import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime)
import Web.Twitter.Types (Status (..), User (..))

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
data LikedUser = LikedUser
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
  }
  deriving (Show)

-- | A container for liked users and tweet counts.
data Liked = Liked
  { users :: Map UserID LikedUser
  , tweetsByUser :: Map UserID Int
  }

-- | Fold a conduit of liked tweets by counting them by author.
likedAuthors :: (Monad m) => ConduitT Status o m Liked
likedAuthors = C.foldl f zero
 where
  zero :: Liked
  zero = Liked{users = Map.empty, tweetsByUser = Map.empty}

  f :: Liked -> Status -> Liked
  f Liked{..} Status{statusUser = User{..}, ..} =
    Liked
      { users = Map.insertWith updateUser userId likedUser users
      , tweetsByUser = Map.insertWith (+) userId 1 tweetsByUser
      }
   where
    likedUser =
      LikedUser
        { screenName = userScreenName
        , displayName = userName
        , isVerified = userVerified
        , createdAt = userCreatedAt
        , followerCount = userFollowersCount
        , followingCount = userFriendsCount
        , tweetCount = userStatusesCount
        , likesCount = userFavoritesCount
        , hasPossiblySensitiveTweets = fromMaybe False statusPossiblySensitive
        }

    updateUser new old =
      old
        { hasPossiblySensitiveTweets =
            hasPossiblySensitiveTweets old || hasPossiblySensitiveTweets new
        }
