module Tweetogram.Query.LikedBy (
  LikedByCount,
  LikedByUsers,
  LikedByUser,
  likedBy,
) where

import Relude

import Data.Conduit (ConduitT)
import Web.Twitter.Types (Status)

import Tweetogram.Shared.User (UserID, UserWith (..))

type LikedByCount = Int

type LikedByUser = UserWith LikedByCount

type LikedByUsers = Map UserID LikedByUser

likedBy :: ConduitT Status o m (UserWith LikedByCount)
likedBy = undefined
