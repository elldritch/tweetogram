-- | Analyze whose tweets a user has liked.
module Tweetogram.Query.Likes (
  LikedTweetsCount,
  LikedUsers,
  LikedUser,
  likedAuthors,
) where

import Relude

import Data.Conduit (ConduitT)
import Data.Conduit.Combinators qualified as C
import Data.Map.Strict qualified as Map
import Web.Twitter.Types (Status (..), User (..))

import Tweetogram.Shared.User (UserID, UserWith (..))

type LikedTweetsCount = Int

type LikedUser = UserWith LikedTweetsCount

type LikedUsers = Map UserID LikedUser

-- | Fold a conduit of liked tweets by counting them by author.
likedAuthors :: (Monad m) => ConduitT Status o m LikedUsers
likedAuthors = C.foldl f Map.empty
 where
  f :: LikedUsers -> Status -> LikedUsers
  f m Status{statusUser = User{..}, ..} =
    Map.insertWith updateUser userId likedUser m
   where
    likedUser =
      TwitterUser
        { screenName = userScreenName
        , displayName = userName
        , isVerified = userVerified
        , createdAt = userCreatedAt
        , followerCount = userFollowersCount
        , followingCount = userFriendsCount
        , tweetCount = userStatusesCount
        , likesCount = userFavoritesCount
        , hasPossiblySensitiveTweets = fromMaybe False statusPossiblySensitive
        , metadata = 1
        }

    updateUser new old =
      old
        { hasPossiblySensitiveTweets =
            hasPossiblySensitiveTweets old || hasPossiblySensitiveTweets new
        , metadata = metadata old + 1
        }
