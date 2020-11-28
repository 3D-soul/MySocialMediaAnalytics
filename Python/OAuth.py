import os
from dotenv import load_dotenv
from requests_oauthlib import OAuth1Session

## ----- GLOBAL VARIABLES -----
folder_path = os.path.expanduser('../data/')
load_dotenv(os.path.join(folder_path, '.env'))

# URL's
default = "https://api.twitter.com/labs/2/users/by?"
users_tweets = "https://api.twitter.com/2/tweets/search/recent?query=from:821Suhruth"
request_token_url = "https://api.twitter.com/oauth/request_token"
base_authorization_url = "https://api.twitter.com/oauth/authorize"
access_token_url = "https://api.twitter.com/oauth/access_token"


## ----  Authentication and Authorization  ----
# Loading the keys
consumer_key = os.environ.get("TWITTER_API_KEY")
consumer_secret = os.environ.get("TWITTER_API_SECRET")

fields = "created_at,description,pinned_tweet_id"
params = {"usernames": "821Suhruth", "user.fields": fields}

# Get request token
oauth = OAuth1Session(consumer_key, client_secret=consumer_secret)
fetch_response = oauth.fetch_request_token(request_token_url)
resource_owner_key = fetch_response.get("oauth_token")
resource_owner_secret = fetch_response.get("oauth_token_secret")

# Get authorization
authorization_url = oauth.authorization_url(base_authorization_url)
print(f"Please go here and authorize: {authorization_url}")
verifier = input("Paste the PIN here: ")

# Get the access token
oauth = OAuth1Session(
    consumer_key,
    client_secret=consumer_secret,
    resource_owner_key=resource_owner_key,
    resource_owner_secret=resource_owner_secret,
    verifier=verifier,
)

oauth_tokens = oauth.fetch_access_token(access_token_url)
access_token = oauth_tokens["oauth_token"]
access_token_secret = oauth_tokens["oauth_token_secret"]

# Make the request
oauth = OAuth1Session(
    consumer_key,
    client_secret=consumer_secret,
    resource_owner_key=access_token,
    resource_owner_secret=access_token_secret,
)

response = oauth.get(default, params=params)
print(response)
print(f"Response status: {response.status_code}")
print(f"Body: {response.text}")




