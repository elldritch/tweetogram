-- | Analyze the activity of tweet authors.
module Tweetogram.Query.Activity (
  Hour,
  tweetActivity,
) where

import Data.Map qualified as Map
import Data.Time (TimeOfDay (..), UTCTime (..), timeToTimeOfDay)

import Conduit (foldlC)
import Data.Conduit (ConduitT)

import Web.Twitter.Types (Status (..))

import Relude

-- | An Int from 0 to 23 (inclusive) indicating the hour time-of-day when a
-- tweet was posted.
type Hour = Int

-- | Fold a conduit of tweets into a map from the time-of-day when a tweet was
-- posted to the number of tweets posted during that time-of-day.
tweetActivity :: (Monad m) => ConduitT Status o m (Map Hour Int)
tweetActivity = foldlC f $ Map.fromList $ zip [0 .. 23] $ repeat 0
 where
  f :: Map Hour Int -> Status -> Map Hour Int
  f m Status{statusCreatedAt = UTCTime{utctDayTime = dayTime}} =
    Map.insertWith (+) hour 1 m
   where
    TimeOfDay{todHour = hour} = timeToTimeOfDay dayTime
