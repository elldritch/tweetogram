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


# Once your liked tweets are downloaded, you can render your rankings.
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
                           Your "Consumer Keys: API Key" from the Twitter
                           Developer Portal
  --twitter-consumer-api-key-secret ARG
                           Your "Consumer Keys: API Key Secret" from the Twitter
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

Here's a truncated sample of what the output looks like:

```
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| Rank | Liked tweets |     Handle      |                       Name                        | Verified? | NSFW*? | Followers | Following | Tweets | Likes  |         Created         |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 1    | 253          | GergelyOrosz    | Gergely Orosz                                     | False     | False  | 33267     | 647       | 8326   | 9943   | 2009-04-10 10:01:11 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 2    | 101          | TrungTPhan      | Trung Phan 🇨🇦                                     | False     | False  | 201756    | 3270      | 24646  | 49983  | 2017-12-27 00:42:32 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 3    | 77           | patio11         | Patrick McKenzie                                  | False     | False  | 111716    | 735       | 45179  | 21865  | 2009-02-14 12:26:59 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 4    | 64           | BretDevereaux   | Bret Devereaux                                    | False     | False  | 12660     | 256       | 9062   | 4122   | 2019-05-02 18:26:19 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 5    | 53           | joulee          | Julie Zhuo                                        | True      | False  | 122545    | 845       | 4603   | 7062   | 2008-11-25 03:58:28 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 6    | 52           | justinkan       | Justin Kan                                        | True      | False  | 211353    | 2065      | 13262  | 18401  | 2009-04-05 01:55:05 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 7    | 45           | paulg           | Paul Graham                                       | True      | False  | 1384437   | 645       | 26178  | 46957  | 2010-08-27 20:13:59 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 8    | 43           | dave_universetf | Dave Anderson                                     | False     | False  | 5145      | 236       | 8895   | 13231  | 2017-12-02 19:31:20 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 9    | 39           | FrogandToadbot  | Frog and Toad                                     | False     | False  | 110484    | 7         | 2676   | 367    | 2020-12-30 07:11:16 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 10   | 38           | chrishlad       | Chris Hladczuk                                    | False     | False  | 97193     | 349       | 5710   | 13954  | 2020-06-18 23:27:47 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 11   | 37           | sweatystartup   | Nick Huber                                        | False     | False  | 157577    | 1240      | 11592  | 25672  | 2018-11-28 23:46:15 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 12   | 36           | RomeenSheth     | Romeen Sheth                                      | False     | False  | 52694     | 1386      | 5725   | 22220  | 2012-10-20 18:59:34 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 13   | 36           | jspujji         | Jesse Pujji                                       | True      | False  | 50499     | 3588      | 8321   | 18612  | 2008-05-23 07:49:31 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 14   | 36           | rakyll          | Jaana Dogan ヤナ ドガン                                | True      | False  | 73260     | 1084      | 16159  | 86769  | 2007-12-03 13:08:02 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 15   | 33           | kevinleeme      | Kevin Lee                                         | False     | False  | 23688     | 570       | 5276   | 16647  | 2008-12-10 20:47:48 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 16   | 29           | hillelogram     | Hillel                                            | False     | False  | 14767     | 3         | 42183  | 44656  | 2011-10-25 04:31:15 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 17   | 27           | DeepLeffen      | Deep Leffen Bot                                   | False     | False  | 97739     | 86        | 529    | 1518   | 2020-03-22 23:25:32 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 18   | 27           | PossumEveryHour | Possum Every Hour                                 | False     | False  | 384747    | 1         | 27318  | 0      | 2018-07-25 12:01:37 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 19   | 26           | Codie_Sanchez   | Codie Sanchez 💥                                   | False     | False  | 71177     | 574       | 6166   | 10489  | 2011-02-27 22:07:36 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
| 20   | 26           | gentleeeeeeecat | GENTLECAT                                         | False     | False  | 117830    | 47        | 841    | 1445   | 2017-01-03 13:43:06 UTC |
+------+--------------+-----------------+---------------------------------------------------+-----------+--------+-----------+-----------+--------+--------+-------------------------+
```
