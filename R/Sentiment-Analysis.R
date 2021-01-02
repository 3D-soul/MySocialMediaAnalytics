## Libraries
library(tm)
library(stringr)
library(twitteR)
library(wordcloud)
library(randomcoloR)


library(ggmap)
library(ggplot2)
library(dplyr)

library(lubridate)
library(data.table)



;### --- Sentiment Analysis ---
## Questions 
# What kind of tweets they tweet? Basically, sentiment
# What are the major topics they tweet about?
# Can we identify certain themes from their tweets?

# authentication
consumerKey <- TWITTER_API_KEY
consumerSecret <- TWITTER_API_SECRET
accessToken <- TWITTER_ACCESS_TOKEN
accessTokenSecret <- TWITTER_ACCESS_SECRET

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

## analyze tweets of obama
# getting tweeets
tweetsList <- userTimeline("BarackObama", 1500)

# convert to df
tweetsDF <- twListToDF(tweetsList)
tweetsDF <- as_tibble(tweetsDF)
tweetsDF

## cleaning data
all_handles <- str_replace_all(tweetsDF$text, "@\\w+", "")
all_handles <- gsub("https\\S*", "", all_handles)
all_handles <- gsub("@\\S*", "", all_handles)
all_handles <- gsub("[\r\n]", "", all_handles)
all_handles <- gsub("[[:punct:]]", "", all_handles)
all_handles <- gsub("\\d+", "", all_handles)
all_handles <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", all_handles)
all_handles <- tolower(all_handles)
all_handles

# removed stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')

my_docs = stringr::str_replace_all(all_handles, stopwords_regex, '')

# remove white spaces
my_docs <- str_trim(my_docs, side = c("both", "left", "right"))
my_docs <- str_squish(my_docs)

my_docs

myCorpus <- Corpus(VectorSource(my_docs)) # create corpus

# getting the words df
dtm <- TermDocumentMatrix(myCorpus)
mat <- as.matrix(dtm)
words <- sort(rowSums(mat), decreasing=T)

df <- data.frame(word = names(words), freq=words)
df

df$word

# plotting word cloud of obama tweets
myColor <- randomcoloR::distinctColorPalette(k = 1000) # getting a color palette

wordcloud(words = myCorpus,
          scale = c(2.5, 0.1),
          max.words = 1000,
          random.order = FALSE,
          rot.per = 0.35,
          use.r.layout = FALSE,
          colors = myColor)


