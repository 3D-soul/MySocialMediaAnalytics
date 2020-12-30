## --- OVERVIEW ---
# load the package
library(twitteR)

# authentication
consumerKey <- TWITTER_API_KEY
consumerSecret <- TWITTER_API_SECRET
accessToken <- ACCESS_TOKEN
accessTokenSecret <- ACCESS_TOKEN_SECRET

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

# testing
user <- getUser("kajalagarwal")
tweets <- userTimeline(user, n=10)
tweets

tweets[[1]]$getClass()
tweets[[1]]$favoriteCount


## --- Trends Analysis ---
library(tm)
library(ggmap)
library(ggplot2)
library(twitteR)
library(stringr)
library(wordcloud)
library(lubridate)
library(data.table)

# authentication
consumerKey <- TWITTER_API_KEY
consumerSecret <- TWITTER_API_SECRET
accessToken <- ACCESS_TOKEN
accessTokenSecret <- ACCESS_TOKEN_SECRET

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

# extract tweets based on search
searchTerm <- "#earthquake"
trendingTweets = searchTwitter(searchTerm, n=1000)


# cleaning earth quake data
trendingTweets.df = twListToDF(trendingTweets)

trendingTweets.df$text <- sapply(trendingTweets.df$text,
                                 function(x) iconv(x, to="UTF-8"))

trendingTweets.df$created <- ymd_hms(trendingTweets.df$created)


## Questions
# - When was the earthquake reported?
# - Where were some of the most recent earthquakes?
# - What devices/services were used to report earthquake related tweets?
# - Which agencies/accounts are to be trusted?


# plot on tweets by time
ggplot(data = trendingTweets.df, aes(x=created)) +
    geom_histogram(aes(fill = ..count..)) +
    theme(legend.position = "none") +
    xlab("Time") + ylab("Number of tweets") +
    scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# checking if location helps 
sapply(trendingTweets.df, function(x) sum(is.na(x)))

# identify countries
library(maps)
library(plyr)

## func to get country name from string
df <- trendingTweets.df$text

cleaned_df <- gsub("[^[:alnum:]]", " ", as.matrix(tmp_df))

split_df <- strsplit(cleaned_df, '\\s+')


get_country <- function(x){
    
    res <- x[which(x %in% world.cities$country.etc)]
    
    if (identical(res, character(0))){ return("rest_of_the_world") }
    else { return(res) }
}


trendingTweets.df$quakeCountry <- llply(split_df, get_country)


# ======= Not working ========== 
# also can't save the data trending.df
## plot tweets by counts
ggplot(subset(trendingTweets.df,quakeCountry != 'rest_of_the_world'),aes(y=retweetCount, x=quakeCountry)) +
    geom_bar(fill = "aquamarine4", stat = "identity") +
    theme(legend.position="none", axis.title.x = element_blank()) +
    ylab("Number of tweets") +
    ggtitle("Tweets by Country")


ggplot(subset(trendingTweets.df,quakeCountry != 'rest_of_the_world'))+
    aes(x=quakeCountry, y=retweetCount)+
    geom_col()








