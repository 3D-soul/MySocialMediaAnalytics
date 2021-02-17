library(dplyr)
library(ggplot2)
library(lubridate)

gamedev_sx_df <- tibble(read.csv(file.choose()))[,-1]
gamedev_sx_df


## Baisc questions
# number of posts
dim(gamedev_sx_df) 

# number of questions
sum(gamedev_sx_df$PostTypeId == 1)

# number of answer per question
dim(gamedev_sx_df[gamedev_sx_df$PostTypeId == 2,])[1]/
    dim(gamedev_sx_df[gamedev_sx_df$PostTypeId == 1,])[1]

### ----------------------------------------------------
## No of posts by year and month
gamedev_sx_df$Date <- date(gamedev_sx_df$CreationDate)

# plotting date vs No of posts
ggplot(data = gamedev_sx_df, aes(x=Date)) +
    geom_histogram(binwidth = 50, aes(fill = ..count..)) +
    xlab("Date") + ylab("Number of posts") +
    ggtitle("No of Posts by Date") +
    theme(plot.title=element_text(hjust=0.5))
    theme_bw()

## No of posts by Tag Info
info_list <- c("c#", "c", "c++", "python", "r", "objective-c",
               "actionscript-3", "2d", "3d", "2,5d")
    
get_tag_info <- function(x) {
    
    if (!is.na(x[1])){
        for (st in x){
            for (i in 1:length(st)){
                if (st[i] %in% info_list){
                    return(st[i])
                }
            }
           
        }
    }
    return("rest_of_the_world")
}    
    
    
gamedev_sx_df$taglist <- lapply(str_split(gamedev_sx_df$Tags, "<|>"), 
                                function(x){x %>% unlist()}) %>%
                            lapply(., function(x) x[x != ""])

gamedev_sx_df$TagInfo <- as.character(lapply(gamedev_sx_df$taglist, get_tag_info))


infoDF <- gamedev_sx_df[, c("Date", "TagInfo")]

infoDF <- infoDF[infoDF$TagInfo != "rest_of_the_world",]

infoDF$Date <- format(infoDF$Date, '%b-%Y')

aggDF <- aggregate(infoDF$TagInfo, 
                   by = list(infoDF$Date, infoDF$TagInfo),
                   length)

colnames(aggDF) <- c("date", "tag", "count")

aggDF$date <- as.Date(paste("01", aggDF$date, sep="-"), "%d-%b-%Y")

agglangDF <- aggDF[!((aggDF$tag == "2d") | (aggDF$tag == "3d")),]

# plotting tag info by time
ggplot(agglangDF, aes(x=date, y=count, group=tag))+
    geom_point(aes(shape=tag)) +
    geom_line(aes(color=tag)) +
    xlab("Date") + ylab("Count") +
    ggtitle("Language wise post count over Time") +
    theme_bw() +
    theme(plot.title = element_text(hjust =0.5)) 

"
- The plot clearly depicts that c# and c++ are the main languages of game development
"    



# plotting without c# and c++
new_aggDF <- aggDF[!((aggDF$tag == "c#") | (aggDF$tag == "c++") | (aggDF$tag == "2d") | (aggDF$tag == "3d")),]

ggplot(new_aggDF, aes(x=date, y=count, group=tag))+
    geom_point(aes(shape=tag)) +
    geom_line(aes(color=tag)) +
    xlab("Date") + ylab("Count") +
    labs(title = "Language wise post count over Time",
         subtitle = "except c# and c++") +
    theme_bw() +
    theme(plot.title = element_text(hjust =0.5),
          plot.subtitle = element_text(hjust =0.5)) 

"
- All of the other lamnguages had a good impact in 2010-2015 and thereby decreased drastically
"

# trends of 2d vs 3dplotting 
new_aggDF <-  aggDF[((aggDF$tag == "2d") | (aggDF$tag == "3d")),]

ggplot(new_aggDF, aes(x=date, y=count, group=tag))+
    geom_point(aes(shape=tag)) +
    geom_line(aes(color=tag)) +
    xlab("Date") + ylab("Count") +
    labs(title = "Post Count over Time",
         subtitle = "2d VS 3d") +
    theme_bw() +
    theme(plot.title = element_text(hjust =0.5),
          plot.subtitle = element_text(hjust =0.5))
    
"
- Comparing 2d and 3d, 2d has more posts count by time
- Only once between 2010-2015 3d count was more than 2d
"


