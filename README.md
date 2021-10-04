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

For more options, see `--help`:

```
$ cabal run tweetogram -- --help
Usage: tweetogram COMMAND
  Compute statistics about your liked tweets

Available options:
  -h,--help                Show this help text

Available commands:
  download                 Download your liked tweets
  query                    Query statistics about liked tweets
```

```
$ cabal run tweetogram -- download --help
Usage: tweetogram download --twitter-consumer-api-key ARG
                           --twitter-consumer-api-key-secret ARG
                           --twitter-access-token ARG
                           --twitter-access-token-secret ARG
                           --twitter-username ARG --data-dir ARG
  Download your liked tweets

Available options:
  --twitter-consumer-api-key ARG
                           Your "Consumer Key: API Key" from the Twitter
                           Developer Portal
  --twitter-consumer-api-key-secret ARG
                           Your "Consumer Key: API Key Secret" from the Twitter
                           Developer Portal
  --twitter-access-token ARG
                           Your "Authentication Tokens: Access Token" from the
                           Twitter Developer Portal
  --twitter-access-token-secret ARG
                           Your "Authentication Tokens: Access Token Secret"
                           from the Twitter Developer Portal
  --twitter-username ARG   Username of the account to download liked tweets from
  --data-dir ARG           Filepath to a directory to save downloaded tweets
  -h,--help                Show this help text
```

```
$ cabal run tweetogram -- query --help
Usage: tweetogram query --data-dir ARG [--top N] [--min-likes N]
  Query statistics about liked tweets

Available options:
  --data-dir ARG           Filepath to a directory containing downloaded tweets
  --top N                  Only show top N most liked accounts
  --min-likes N            Only show accounts with at least N likes
  -h,--help                Show this help text
```
