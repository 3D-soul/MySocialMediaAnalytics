import os
import tweepy
from dotenv import load_dotenv


## --- Global Variables ---
# usernames
KAJAL = "MsKajalAggarwal"
BEER_BICEPS = "BeerBicepsGuy"
MY_ACC = "821Suhruth"

# url's
SEARCH = "https://api.twitter.com/2/tweets/search/stream"
REST = "https://api.twitter.com/1.1/search/tweets.json"
STREAM = "https://stream.twitter.com/1.1/statuses/sample.json"

## Loading the environment variables
path = os.path.expanduser("../data/")
load_dotenv(os.path.join(path, ".env"))

consumer_key, consumer_secret = os.getenv("TWITTER_API_KEY"), os.getenv("TWITTER_API_SECRET")
access_token, access_secret = os.getenv("TWITTER_ACCESS_TOKEN"), os.getenv("TWITTER_ACCESS_SECRET")


auth = tweepy.OAuthHandler(consumer_key,consumer_secret)
auth.set_access_token(access_token, access_secret)

api = tweepy.API(auth)





