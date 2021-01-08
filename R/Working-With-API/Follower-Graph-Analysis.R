### --- Follower Graph  analysis ---
library(igraph)
library(dplyr)
library(twitteR)
library(data.table)
library(RColorBrewer)

## authentication
consumerKey <- TWITTER_API_KEY
consumerSecret <- TWITTER_API_SECRET
accessToken <- TWITTER_ACCESS_TOKEN
accessTokenSecret <- TWITTER_ACCESS_SECRET

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

### ===========================================================================
# function to get follower list
get_follower_list <- function(userName){
    # get user data
    twitterUser <- getUser(userName)
    twitterUserFollowerIDs<-twitterUser$getFollowers(retryOnRateLimit=1)
    
    # extract the list of followers
    return (sapply(twitterUserFollowerIDs,screenName))
}

# append rows to dataframe
append_to_df<-function(dt, elems)
{ 
    return(rbindlist(list(dt,  elems),use.names = TRUE))
}

### ========================================================================

## Analyze RdPeng Followers
# getting the user name
UserName <- "rdpeng"
twitterUser <- getUser(UserName)

# extract followers
twitterUser_follower_IDs <- twitterUser$getFollowers(retryOnRateLimit=10)
twitterUser_follower_IDs

# converting to a dataframe
twitterUser_follower_df = rbindlist(lapply(twitterUser_follower_IDs, as.data.frame))
twitterUser_follower_df <- as_tibble(twitterUser_follower_df)
twitterUser_follower_df

# filter followers
filtered_df <- subset(twitterUser_follower_df, 
                      followersCount < 100 & 
                          statusesCount > 100 & 
                          statusesCount < 5000 & 
                          protected==FALSE) 
filtered_df

# extract twitter handles fo followers
filtered_follower_IDs <- filtered_df$screenName
filtered_follower_IDs

# initial edge data frame
edge_df <- data.frame(from=filtered_follower_IDs,
                      to=rep("821Suhruth",
                             length(filtered_follower_IDs)),
                      stringsAsFactors = F)
edge_df


# Iterate to extract followers of followers
# As this is taking lot of time, I'm stopping at counter = 311
counter = 1
for (follower in filtered_follower_IDs){
    # print(follower)
    
    # fetch follower list for current user
    followerScreenNameList <- get_follower_list(follower)
    
    print(paste("Processing completed for:",
                follower,
                "(", counter,"/",
                length(filtered_follower_IDs), ")"
    ))
    
    # append to edge list
    edge_df <- append_to_df(edge_df,
                            list(from=followerScreenNameList,
                                 to=rep(follower,
                                        length(followerScreenNameList))))
    counter <- counter + 1
}

# prepare network object
net <- graph.data.frame(edge_df, directed = T)

# simplify network
net <- simplify(net, remove.multiple = F, remove.loops = T)

# adjust the size of nodes based on in and out degrees
deg <- degree(net, mode="all")
V(net)$size <- deg * 0.05 +1
V(net)[name == "821Suhruth"]$size <- 15

# node coloring
pal3 <- brewer.pal(10, "Set3")

# overall follower graph
plot(net, 
     edge.arrow.size=0.01,
     vertex.label = ifelse(V(net)$size >= 15
                           ,V(net)$name, NA),
     vertex.color = pal3)


## RDPeng data is so vast that the graph is a sphere.
## Hence using BlenderSwap

# getting the user name
UserName <- "blendswap"
twitterUser <- getUser(UserName)

# extract followers
twitterUser_follower_IDs <- twitterUser$getFollowers(retryOnRateLimit=10)
twitterUser_follower_IDs

# converting to a dataframe
twitterUser_follower_df <- rbindlist(lapply(twitterUser_follower_IDs, as.data.frame))
twitterUser_follower_df <- as_tibble(twitterUser_follower_df)
twitterUser_follower_df

# filtering large data
filtered_df <- subset(twitterUser_follower_df, 
                       followersCount < 100 & 
                          statusesCount > 1000 & 
                          statusesCount < 5000 & 
                          friendsCount > 1000 &
                           protected==FALSE) 

filtered_df

# extract twitter handles fo followers
filtered_follower_IDs <- filtered_df$screenName
filtered_follower_IDs

# initial edge data frame
edge_df <- data.frame(from=filtered_follower_IDs,
                      to=rep("BlendSwap",
                             length(filtered_follower_IDs)),
                      stringsAsFactors = F)
edge_df

# Iterate to extract followers of followers
counter = 1
for (follower in filtered_follower_IDs){
    # print(follower)
    
    # fetch follower list for current user
    followerScreenNameList <- get_follower_list(follower)
    
    print(paste("Processing completed for:",
                follower,
                "(", counter,"/",
                length(filtered_follower_IDs), ")"
    ))
    
    # append to edge list
    edge_df <- append_to_df(edge_df,
                            list(from=followerScreenNameList,
                                 to=rep(follower,
                                        length(followerScreenNameList))))
    counter <- counter + 1
}

edge_df <- as_tibble(edge_df)
edge_df$from <- as.character(edge_df$from)
edge_df

# prepare network object
net <- graph.data.frame(edge_df, directed = T)

# simplify network
net <- simplify(net, remove.multiple = F, remove.loops = T)

# adjust the size of nodes based on in and out degrees
deg <- degree(net, mode="all")
V(net)$size <- deg * 0.05 +1
V(net)[name == "BlendSwap"]$size <- 15

# node coloring
pal3 <- brewer.pal(10, "Set3")

# overall follower graph
plot(net, 
     edge.arrow.size=0.1,
     vertex.label = ifelse(V(net)$size >= 15
                           ,V(net)$name, NA),
     vertex.color = pal3)


## Plotting friends nodes
# identify friend vertices
friendVertices <- ends(net, es=E(net)[from("BlendSwap")])
friendVertices[,1]

# Generate edge width variable:
ew <- rep(2, ecount(net))
ew[which (V(net)$name %in% friendVertices)] <- 4
ew

# generate edge color variable
ecol <- rep("grey80", ecount(net))
ecol[which(V(net)$name %in% friendVertices)] <- "red"

# generate node color variable
vcol <- rep("gray80", vcount(net))
vcol[which(V(net)$name %in% friendVertices)] <- "gold"

V(net)$label.cex <- 1.2

plot(net,
     vertex.color = vcol,
     edge.color = ecol,
     edge.width = ew,
     edge.arrow.mode = 0,
     vertex.label = ifelse(V(net)$name %in% friendVertices,
                           V(net)$name, NA),
     vertex.label.color = "black",
     vertext.label.font = 2,
     edge.curved = 0.1) 

## So amoung these 34 people BlendSwap is not following any one of the


### ==================================================================
### Save Data
# reading the data
edge_df <- read.csv("./data/rdpeng_edgedf.csv")
twitterUser_follower_df <- read.csv("./data/rdpeng_data.csv")


## saving the data 
tmp_df <- as_tibble(edge_df)
tmp_df$from <- as.character(tmp_df$from)
tmp_df

write.csv(twitterUser_follower_df, "./data/csv-files/rdpeng_data.csv")
write.csv(tmp_df,"./data/csv-files/rdpeng_edgedf.csv" )
write.csv(edge_df,"./data/csv-files/blendswap.csv" )

### ==================================================================
