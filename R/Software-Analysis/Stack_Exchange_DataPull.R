library(XML)
library(dplyr)
library(stringr)
library(lubridate)


# https://archive.org/details/stackexchange
find_names <- function(List){
    
    mynames <- names(List[1]$row)
    
    for (i in 1:length(List)){
    current_names <- names(List[i]$row)

    for (name in current_names){
       if (!(name %in% mynames)){
           mynames <- c(mynames, name)
           # tmp_list[i]$row[name] <- NA
        }
    }
  }
    return(sort(mynames))
}
        

check_val <- function(List, given_names){
    for (i in 1:length(List)){
        cur_names <- names(List[i]$row)
        for (name in given_names){
            if (!(name %in% cur_names)){
                List[i]$row[name] <- NA
            }
        }
    }
    return(List)
} 


check_order <- function(List, given_names){
    lst <- list()
    
    for (i in 1:length(List)){
        vec <- c()
        for (name in given_names){
           vec <- c(vec, List[i]$row[name][[1]])
        }
        lst[[i]] <- vec
    }
    return(lst)
}


loadXMLToDataFrame <- function(xmlFilePath, col_names){
    doc <- xmlParse(xmlFilePath)
    xmlList <- xmlToList(doc)
    
    col_names <- find_names(xmlList)
    
    finalList <- check_val(xmlList, col_names)
    finalList <- check_order(finalList, col_names)
    
    data <- tibble(do.call(rbind.data.frame, finalList))
    colnames(data) <- col_names

    return (data)
}

eng_postsDF <- loadXMLToDataFrame("./data/xml_files/eng.xml")
eng_postsDF

gamedev_postsDF <- loadXMLToDataFrame("./data/xml_files/gamedev_posts.xml")
gamedev_postsDF

gaming_postsDF <- loadXMLToDataFrame("./data/xml_files/gaming_posts.xml")
gaming_postsDF

### ------------------------------------------------------------------------
## DATA CLEANING
# columns are same for all data frames
sum((colnames(eng_postsDF) != colnames(gamedev_postsDF)))
sum((colnames(gaming_postsDF) != colnames(gamedev_postsDF)))

# NA Values
dim(eng_postsDF)
sum(is.na(eng_postsDF))

## seperating columns with no NA values
get_non_na_names <- function(df){
  non_na_names <- c()
  for (name in colnames(df)){
    val <- sum(is.na(df[name]))
    per_val <- round((val/dim(df))[1] * 100)
    if (per_val == 0){
      non_na_names <- c(non_na_names, name)
    }
  }
  return (non_na_names)
}

get_non_na_df <- function(df){
  req_names <- get_non_na_names(eng_postsDF) 
  req_names
  
  idx <- c()
  for (name in req_names){
    if (grepl("id", tolower(name))){
      idx <- c(idx, name)
    }
  } 
  
  for (name in req_names){
  if (grepl("date", tolower(name))){
    idx <- c(idx, name)
  }
  }
  
  for (name in req_names){
    if (grepl("count", tolower(name))){
      idx <- c(idx, name)
    }
  }
  
  ids <- c(idx, "Body", "Score")
  
  non_na_df <- bind_cols(df[,ids], df[, setdiff(req_names, ids)])
  return(non_na_df)
}

## get the low na values and check their type
get_low_na_df <- function(df){
  tmp_df <- get_non_na_df(df)
  rem_names <- setdiff(colnames(df), colnames(tmp_df))
  
  low_na_names <- c()
  for (name in rem_names){
    val <- sum(is.na(df[name]))
    per_val <- round((val/dim(df))[1] * 100)
    
    if (per_val <= 60 ){
      low_na_names <- c(low_na_names, name)
    }
    
  }
  
  cut_names <- c()
  for (name in low_na_names){
    
    if (grepl("count", tolower(name))){
      val <- median(as.integer(df[name][!(is.na(df[name]))]))
      df[name][is.na(df[name])] <- as.character(val)
      
    }
    
    if (grepl("id", tolower(name))){
      cut_names <- c(cut_names, name)
      
    }
    
    low_na_names <- setdiff(low_na_names, cut_names)
    
  }
  
  return (df[low_na_names])
}

get_final_df <- function(df){
  df1 <- get_non_na_df(df)
  df2 <- get_low_na_df(df)
  
  final_df <- bind_cols(df1, df2)
   
  return (final_df[complete.cases(final_df),])
}

## correcting the type of columns in non_na_df
set_col_type <- function(df){
  for (name in colnames(df)){
    if ((grepl("count", tolower(name))) | (grepl("id", tolower(name)))){
      df[name] <- lapply(df[name], as.integer)
    }
    
    if (grepl("date", tolower(name))){
      df[name] <- lapply(df[name], ymd_hms)
    }
    
    if (name == "Tags"){
      
      df$taglist <- lapply(str_split(df$Tags, "<|>"), function(x){x %>% unlist()}) %>%
                     lapply(., function(x) x[x != ""])
      
      tmp_col <- c()
      for (i in 1:length(df$taglist)){
        tmp_col <- c(tmp_col, paste(unlist(df$taglist[i]), sep='', collapse=', '))
        
      }
      
      df$taglist <- tmp_col
    }
  }
  
  df["Score"] <- lapply(df["Score"], as.integer)
  
  df$Body <- lapply(df$Body, function(x) return(gsub("<.*?>", "", x)))
  df$Body <- lapply(df$Body, function(x) return(gsub("[\r\n]", "", x)))
  
  df$Body <- as.character(df$Body)
  
  return(df)
  
}

final_eng_postsDF <- set_col_type(get_final_df(eng_postsDF))
final_eng_postsDF

final_gamedev_postsDF <- set_col_type(get_final_df(gamedev_postsDF))
final_gamedev_postsDF

final_gaming_postsDF <- set_col_type(get_final_df(gaming_postsDF))
final_gaming_postsDF


### -------------------------------------------------------------------------
## Saving the cleaned data
write.csv(final_gamedev_postsDF, "./data/csv-files/gamedev_sx_posts_data.csv")





















