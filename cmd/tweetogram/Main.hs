module Main (main) where

import Conduit (mapM_C, runConduit, takeC, (.|))
import Control.Lens.Setter ((?~))
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
import Text.Pretty.Simple (pPrint)
import Web.Twitter.Conduit (
  APIRequest,
  Credential (..),
  FavoritesList,
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
  options@Options{twitterUsername} <- execParser argsP

  let twInfo = newTWInfo options
  connMgr <- newManager tlsManagerSettings

  -- TODO: Next, dump these to disk so I won't lose them, then group them and
  -- render.
  runConduit $
    sourceWithMaxId twInfo connMgr (getLikes twitterUsername)
      .| takeC maxPageSize
      .| mapM_C mapMLike
 where
  maxPageSize :: (Num a) => a
  maxPageSize = 200

  getLikes :: Text -> APIRequest FavoritesList [Status]
  getLikes user =
    favoritesList
      (Just (ScreenNameParam $ toString user))
      & #count ?~ maxPageSize
      & #tweet_mode ?~ Extended

  mapMLike :: Status -> IO ()
  mapMLike = pPrint
