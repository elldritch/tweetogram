# tweetogram

Tweetogram creates a ~~histogram~~ ranking of the Twitter accounts that you liked the most.

## Usage

```sh
# First, download your liked tweets. This is a separate step because Twitter's
# API has a really strict rate limit, so we download tweets first and then
# operate on downloaded tweets.

# Make a directory to hold the downloaded tweets.
mkdir /tmp/tweetogram

# Run the downloader. Use your own API keys and access tokens here. Use
# https://developer.twitter.com/en/portal/dashboard to get your own set of keys.
cabal run tweetogram -- download \
  --twitter-consumer-api-key AAAA \
  --twitter-consumer-api-key-secret BBBB \
  --twitter-access-token CCCC \
  --twitter-access-token-secret DDDD \
  --twitter-username YOUR_USERNAME \
  --data-dir /tmp/tweetogram

# Render your rankings.
cabal run tweetogram -- query --data-dir /tmp/tweetogram
```
