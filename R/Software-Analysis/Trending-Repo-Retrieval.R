library(gh)
library(dplyr)
library(httr)
library(jsonlite)


"
- We will use the search API
- We will retrieve trending repositories which were created over the last 3
years: 2018-2020
- The definition of trending repositories in our case will be only those
repositories which have at least 500 stars or more
"

### ------- Functions -----------
DataFramefromCall <- function(x) {
    x <- URLencode(x)
    data <- jsonlite::fromJSON(x)
    items <- data$items
    req_names <- c("id", "name", "full_name", "size", "open_issues",
                   'fork', 'stargazers_count', 'watchers', 'forks',
                   "language", "has_issues", "has_downloads", "has_wiki",
                   "has_page", "created_at", "updated_at", "pushed_at", 
                   "url", "description")
    
    mynames <- c()
    for (name in req_names){
        if (name %in% colnames(items)){
            mynames <- c(mynames, name)
        }
    }
    
    items <- items[,mynames]
    
    Sys.sleep(5)
    return(data.frame(items))
} 

get.trending.repositories <- function(timeline.dates, auth.id, auth.pwd){
    # set parameters
    base_url <- "https://api.github.com/search/repositories?"
    
    api_id_param <- paste0("client_id=", auth.id)
    api_pwd_param <- paste0("client_secret=", auth.pwd)
    arg_sep = '&'
    per_page <- 100
    
    top.repos.df <- data.frame(stringsAsFactors=FALSE)
    pb <- txtProgressBar(min = 0, max = length(timeline.dates), style = 3)
    
    # for each pair of dates in list get all trending repos
    for (i in seq(1, length(timeline.dates), by = 2)){ 
        start_date <- timeline.dates[i]
        end_date <- timeline.dates[i+1]
        query <- paste0("q=created:%22", start_date, "%20..%20", end_date, 
                        "%22%20stars:%3E=500")
        url <- paste0(base_url, query, arg_sep, api_id_param, arg_sep, api_pwd_param)
 
        firstCall <- GET(paste(url)) %>% content()
        total_repos <- min(firstCall$total_count, 1000)
        number_calls <- ceiling(total_repos/per_page)
        
        query2 <- "&per_page=100&page="
        api_calls <- paste(url, query2, as.character(c(1:number_calls)), sep = "")
        
        for (i in 1:number_calls) {
            top.repos.df <- rbind(top.repos.df, DataFramefromCall(api_calls[i]))
            Sys.sleep(2)
        
        }
        Sys.sleep(3)
        setTxtProgressBar(pb, i+1)
    }
    print("---- FINISHED ---")
    return(top.repos.df)
}

loop_to_get_repo_data <- function(){
    i <- 0
    v <- 1
    for (date in dates){
        if(i%%2 == 0){
            req_dates <- c(date, dates[v+1])
            tmp_repos <- get.trending.repositories(timeline.dates = req_dates,
                                                   auth.id = auth.id, auth.pwd = auth.pwd)
            Sys.sleep(5)
            
            tmp <- dim(tmp_repos)

            print(paste("Temporarily downloaded", paste(tmp[1], "x", tmp[2])))

            trending_repos <- rbind(trending_repos, tmp_repos)
            
            tmp <- dim(trending_repos)

            print(paste("Final data dimensions", paste(tmp[1], "x", tmp[2])))
            
            Sys.sleep(5)
        }
        i = i + 1
        v = v + 1
    }
    return(trending_repos)
}

auth.id <- GITHUB_CLIENT_ID
auth.pwd <- GITHUB_CLIENT_SECRET

dates <- c('2018-01-01', '2018-03-31',
           '2018-04-01', '2018-06-30',
           '2018-07-01', '2018-09-30',
           '2018-10-01', '2018-12-31',
           '2019-01-01', '2019-03-31',
           '2019-04-01', '2019-06-30',
           '2019-07-01', '2019-09-30',
           '2019-10-01', '2019-12-31',
           '2020-01-01', '2020-03-31',
           '2020-04-01', '2020-06-30',
           '2020-07-01', '2020-09-30',
           '2020-10-01', '2020-12-31')

trending_repos <- data.frame(stringsAsFactors = F)

trending_repos <- loop_to_get_repo_data()

typeof(trending_repos)

trending_repos_df <- data.frame(trending_repos)
trending_repos_df <- as_tibble(trending_repos_df)
trending_repos_df


# saving data
f <- file("./data/trending-repo.csv", "wb")
write.csv(trending_repos_df, file=f, eol="\n")
close(f)

# loading data
trending_repos_df <- read.csv("./data/trending-repo.csv")
trending_repos_df <- as_tibble(trending_repos_df)
trending_repos_df











