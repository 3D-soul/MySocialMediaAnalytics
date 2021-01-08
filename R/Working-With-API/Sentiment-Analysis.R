## Libraries
library(tm)
library(dplyr)
library(ggplot2)
library(stringr)
library(twitteR)
library(wordcloud)
library(data.table)
library(randomcoloR)


### --- Sentiment Analysis ---
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
          scale = c(2.25, 0.1),
          max.words = 1000,
          random.order = FALSE,
          rot.per = 0.35,
          use.r.layout = FALSE,
          colors = myColor)

## --- POlarity Analysis ---
library(syuzhet)

encodeSentiment <- function(x) {
    if(x <= -0.5){
        "very negative"
    }else if(x > -0.5 & x < 0){
        "negative"
    }else if(x > 0 & x < 0.5){
        "positive"
    }else if(x >= 0.5){
        "very positive"
    }else {
        "neutral"
    }
}

tweetSentiments <- get_sentiment(tweetsDF$text, method="syuzhet")
tweets <- as_tibble(cbind(tweetsDF, tweetSentiments))
tweets$sentiment <- sapply(tweets$tweetSentiments, encodeSentiment)
tweets

# plotting the sentiment graph
qplot(tweets$tweetSentiments, bins=30) +
    theme(legend.position="none", 
          plot.title = element_text(hjust = 0.5)) +
    xlab("Sentiment Score") +
    ylab("Number of tweets") +
    ggtitle("Tweets by Senitment Score")

" => The plot is almost symmetric and haence the sentiment is neutral. 
Also there are some outliers towards negative side"


# binning sentiment by category
ggplot(tweets, aes(sentiment))+
    geom_bar(fill="aquamarine4")+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    ylab("Number of tweets")+
    ggtitle("Tweets by Senitment")

## NRC / EmoLex
# Word Emotion Association  Lexion
tweetSentiments <- get_nrc_sentiment(tweetsDF$text)
tweets <- cbind(tweetsDF, tweetSentiments)

sentimentTotals <- data.frame(colSums(tweets[,c(17:26)]))

names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

sentimentTotals

# plotting the EmoLex of Obama tweets
myColor <- randomcoloR::distinctColorPalette(k = 10)

ggplot(sentimentTotals, aes(x = sentiment, y = count))+
    geom_bar(fill = myColor, stat = "identity")+
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))+
    xlab("Sentiment") + ylab("Total count") +
    ggtitle("Total Sentiment Scores")+
    scale_x_discrete(guide = guide_axis(n.dodge=3))


## --- Hierarchical clustering ---
library(tm)

# compute term document matrix
twtrTermDocMat <-  removeSparseTerms(dtm, sparse = 0.97)
tweet_matrix <- as.matrix(twtrTermDocMat)

# creating distance matrix
distMat <- dist(scale(tweet_matrix)) 
# clustering
fit <- hclust(distMat, method = "single")

# plotting the dendrogram
plot(fit)
