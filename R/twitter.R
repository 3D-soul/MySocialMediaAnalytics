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
library(dplyr)
library(ggmap)
library(ggplot2)
library(twitteR)
library(lubridate)
library(data.table)

# authentication
consumerKey <- TWITTER_API_KEY
consumerSecret <- TWITTER_API_SECRET
accessToken <- TWITTER_ACCESS_TOKEN
accessTokenSecret <- TWITTER_ACCESS_SECRET

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

## --- Earth Quake ---
# extract tweets based on search
searchTerm <- "#earthquake"
trendingTweets = searchTwitter(searchTerm, n=1000)


# cleaning earth quake data
trendingTweets.df = twListToDF(trendingTweets)

trendingTweets.df$text <- sapply(trendingTweets.df$text,
                                 function(x) iconv(x, to="UTF-8"))

trendingTweets.df$created <- ymd_hms(trendingTweets.df$created)

trendingTweets.df <- as_tibble(trendingTweets.df)


## Questions
# - When was the earthquake reported?
# - Where were some of the most recent earthquakes?
# - What devices/services were used to report earthquake related tweets?
# - Which agencies/accounts are to be trusted?


# plot on tweets by time
ggplot(data = trendingTweets.df, aes(x=created)) +
    geom_histogram(aes(fill = ..count..)) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    xlab("Time") + ylab("Number of tweets") +
    ggtitle("Tweet Counts by Time") +
    scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

" => Insights: earth quake was largely reported around times 3:00 to 4:00 "  

# checking if location helps 
sapply(trendingTweets.df, function(x) sum(is.na(x)))

" => out of 1000 tweets 867 tweets(86.7%) are NA Values for latitude and longitude
and hence this won't work "


## identify countries
# func to get country name from string
library(maps)
mapCountry <- function(st){
    
    clean_st <- gsub("[^[:alnum:]]", " ", as.matrix(st))
    
    x <- strsplit(clean_st, '\\s+')
    
    for (y in x){
        
        res <- y[which(y %in% world.cities$country.etc)]
        
        if (identical(res, character(0))){ return("rest_of_the_world") }
        else { return(res) }
        
    }
}

trendingTweets.df$Country <- lapply(trendingTweets.df$text, mapCountry)

trendingTweets.df$Country <- as.character(trendingTweets.df$Country)

trendingTweets.df

# getting the unique country list
countries_lst <- subset(trendingTweets.df, Country != 'rest_of_the_world')$Country
cleaned_lst <- gsub("\\s*\\([^\\)]+\\)","",as.character(countries_lst)) 
unique_lst <- unique(cleaned_lst)
unique_lst <- unique_lst[-4]

## plot tweets by counts
ggplot(subset(trendingTweets.df, Country %in% unique_lst),
       aes(Country)) +
    geom_bar(fill = "aquamarine4") +
    theme(legend.position="none", axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ylab("Number of tweets") +
    ggtitle("Tweets by Country") +
    scale_x_discrete(guide = guide_axis(n.dodge=3))
    
" => From the above graph, Croatia is beats all of the others with 
largest no of eathqakes reported"    


## plot tweets by counts
# removing croatia
new_unique_lst <- unique_lst[-1]
ggplot(subset(trendingTweets.df, Country %in% new_unique_lst),
       aes(Country)) +
    geom_bar(fill = "aquamarine4") +
    theme(legend.position="none", axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ylab("Number of tweets") +
    ggtitle("Tweets by Country") +
    scale_x_discrete(guide = guide_axis(n.dodge=3))

" => After Croatia, Fiji and Indonesia seems to had 
large number of tweets  "


## geocode tweets -> map to earthquake locations
# getting country codes using geonames api
library(rjson)
library(geonames)

# authorize geonames
options(geonamesUsername="suhruth")
options(geonamesHost="api.geonames.org")


get_country_code <- function(x){
    df <- GNsearch(name_equals = x)
    res <- c(x, as.numeric(df[1,]$lat), as.numeric(df[1,]$lng))
    
    return(res)
    
}

geoCodedCountries <- lapply(cntry, get_country_code)

# changing list to data frame
geoCodedCountries.df <- data.frame(matrix(unlist(geoCodedCountries), nrow=length(geoCodedCountries), byrow=T))

names(geoCodedCountries.df) <- c("Country", "lat", "lng")

geoCodedCountries.df <- as_tibble(geoCodedCountries.df)

geoCodedCountries.df$lat <- as.numeric(geoCodedCountries.df$lat)
geoCodedCountries.df$lng <- as.numeric(geoCodedCountries.df$lng)

geoCodedCountries.df

## plotting all countries
# getting all countries which are unique 
UniqueCountries <- unique(sort(cleaned_lst))[-2] # unique countries
all_countries <- subset(trendingTweets.df, Country %in% UniqueCountries)$Country

# plotting the geo code
country.x <- geoCodedCountries.df$lng
country.y <- geoCodedCountries.df$lat

mapWorld <- borders("world", colour = "gray50", fill = "gray50")
mp <- ggplot() + mapWorld

mp + geom_point(aes(x=country.x, y=country.y), color="orange", size=sqrt(table(sort(all_countries))))+
    ggtitle("Tweets on World Map") +
    ylab("Latitude") + xlab("Longitude")+
    theme(plot.title = element_text(hjust = 0.5))

## removing 'croatica' from plot
# getting all countries which are unique 
UniqueCountries <- unique(sort(cleaned_lst))[-2][-3] # unique countries
all_countries <- subset(trendingTweets.df, Country %in% UniqueCountries)$Country

# plotting the geo code
country.x <- geoCodedCountries.df[-3,]$lng
country.y <- geoCodedCountries.df[-3,]$lat

mapWorld <- borders("world", colour = "gray50", fill = "gray50")
mp <- ggplot() + mapWorld

mp + geom_point(aes(x=country.x, y=country.y), color="orange", size=sqrt(table(sort(all_countries))))+
    ggtitle("Tweets on World Map") +
    ylab("Latitude") + xlab("Longitude")+
    theme(plot.title = element_text(hjust = 0.5))


## Which sources are generating tweets in my data?
get_source_name <- function(x){
    res <- x %>% 
        read_html() %>% 
        html_text()
    
    if (identical(res, character(0))){ return("Others") }
    else { return(res) }
}

trendingTweets.df$tweetSource <- lapply(tmp, get_source_name)

trendingTweets.df$tweetSource <- as.character(trendingTweets.df$tweetSource)

# sum(trendingTweets.df$tweetSource == "Others") # there are not nan values

# plotting
ggplot(trendingTweets.df, aes(tweetSource))+
    geom_bar(fill="aquamarine4") +
    theme(legend.position = "none", axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_text(hjust = 0.5))+
    ylab("Number of tweets") +
    ggtitle("Tweets by Source")

"=> There are many non-human sources, yet android users are generating more tweets"

## creating a world cloud
library(tm)
library(stringr)
library(wordcloud)

all_handles <- str_extract_all(trendingTweets.df$text, "@\\w+")
all_handles <- all_handles[lapply(all_handles,length)>0]
all_handles <- gsub("\\s*\\([^\\)]+\\)","",as.character(all_handles)) 
all_handles <- sort(all_handles)[1:598]

namesCorpus <- Corpus(VectorSource(all_handles))

library(randomcoloR)
myColor <- randomcoloR::distinctColorPalette(k = 100) # getting a color palette

set.seed(42)
wordcloud(words = namesCorpus,
          scale = c(3, 0.5),
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.10,
          use.r.layout = TRUE,
          colors = myColor)




