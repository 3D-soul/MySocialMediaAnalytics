import os
import tweepy
from dotenv import load_dotenv

## --- Global Variables ---
# usernames
KAJAL = "MsKajalAggarwal"
BEER_BICEPS = "BeerBicepsGuy"
MY_ACC = "821Suhruth"

## Loading the environment variables
path = os.path.expanduser("../data/")
load_dotenv(os.path.join(path, ".env"))

consumer_key, consumer_secret = os.getenv("TWITTER_API_KEY"), os.getenv("TWITTER_API_SECRET")
access_token, access_secret = os.getenv("TWITTER_ACCESS_TOKEN"), os.getenv("TWITTER_ACCESS_SECRET")

# [print(x) for x in [consumer_key, consumer_secret, access_token, access_secret]]

## --- Hello Tweepy ---
auth = tweepy.OAuthHandler(consumer_key,consumer_secret)
auth.set_access_token(access_token, access_secret)

api = tweepy.API(auth)

public_tweets = api.home_timeline()
# for tweet in public_tweets:
#     print(tweet.text)


## --- Models ---
user = api.get_user(KAJAL)

# print(user.screen_name)
# print(user.followers_count)
# for friend in user.friends():
#    print(friend.screen_name)

