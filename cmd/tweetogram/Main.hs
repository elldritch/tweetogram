module Main (main) where

import Conduit (
  ConduitT,
  PrimMonad,
  concatMapCE,
  conduitVector,
  runConduit,
  runResourceT,
  sinkFileCautious,
  (.|),
 )
import Control.Lens.Setter ((?~))
import Data.Aeson (encode)
import Data.Conduit.Throttle (
  Conf,
  newConf,
  setInterval,
  setMaxThroughput,
  throttleProducer,
 )
import Data.Vector (Vector)
import Options.Applicative (
  Parser,
  ParserInfo,
  execParser,
  fullDesc,
  helper,
  info,
  long,
  progDesc,
  strOption,
 )
import Relude
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
import Web.Twitter.Types (Status (..))

data Options = Options
  { twitterConsumerKey :: ByteString
  , twitterConsumerSecret :: ByteString
  , twitterAccessToken :: ByteString
  , twitterAccessTokenSecret :: ByteString
  , twitterUsername :: Text
  , dataDir :: FilePath
  }
  deriving (Show)

optionsP :: Parser Options
optionsP =
  Options
    <$> strOption (long "twitter-consumer-api-key")
    <*> strOption (long "twitter-consumer-api-key-secret")
    <*> strOption (long "twitter-access-token")
    <*> strOption (long "twitter-access-token-secret")
    <*> strOption (long "twitter-username")
    <*> strOption (long "data-dir")

argsP :: ParserInfo Options
argsP = info (optionsP <**> helper) (fullDesc <> progDesc "Computes which accounts you like the most tweets from")

newTWInfo :: Options -> TWInfo
newTWInfo Options{..} =
  setCredential
    (twitterOAuth{oauthConsumerKey = twitterConsumerKey, oauthConsumerSecret = twitterConsumerSecret})
    (Credential [("oauth_token", twitterAccessToken), ("oauth_token_secret", twitterAccessTokenSecret)])
    def

main :: IO ()
main = do
  options@Options{..} <- execParser argsP

  let twInfo = newTWInfo options
  connMgr <- newManager tlsManagerSettings

  runResourceT $
    runConduit $
      throttleProducer throttleConf (getLikes twInfo connMgr twitterUsername)
        .| concatMapCE ((<> "\n") . toStrict . encode)
        .| sinkFileCautious dataDir
 where
  maxPageSize :: (Num a) => a
  maxPageSize = 200

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
      .| conduitVector maxPageSize

  getLikesReq :: Text -> APIRequest FavoritesList [Status]
  getLikesReq user =
    favoritesList
      (Just (ScreenNameParam $ toString user))
      & #count ?~ maxPageSize
      & #tweet_mode ?~ Extended
