library(dplyr)
library(sqldf)
library(data.table)
library(ggplot2)
library(corrplot)
library(reshape2)
library(lubridate)
library(hrbrthemes)

# loading data
trending_repos_df <- read.csv("./data/trending-repo.csv")
trending_repos_df <- as_tibble(trending_repos_df)
trending_repos_df

### --- Trending Repos Created Over Time ---
# extract date elements
trending_repos_df$created_year <- format(as.Date(trending_repos_df$created_at), "%Y")
trending_repos_df$created_monthyear <- format(as.Date(trending_repos_df$created_at), "%b-%Y")

trending_repos_df %>%
    select(created_year, created_monthyear)

# counting total by month and year
repos_by_created_time <- aggregate(trending_repos_df$created_monthyear,
                                   by=list(trending_repos_df$created_monthyear), length)
colnames(repos_by_created_time) <- c("createdTime", "Count")

# format dates
repos_by_created_time$createdTime <- mdy(repos_by_created_time$createdTime)

repos_by_created_time

# Plotting total repo by month and year
ggplot(repos_by_created_time, aes(x=createdTime, y=Count)) +
    geom_line(aes(color=Count), size=1.5) +
    scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y") +
    geom_text(aes(label=Count), vjust=0.3, position=position_dodge(.9), size=3) +
    labs(x="Created Time", y="Trending Repository Count", 
         title = "Trending Repositiories vs Created Time",
         subtitle = "Total trending repositories created in GitHub over time") +
    theme_ipsum_rc(grid='XY', axis_title_just = 0.5)+
    theme(legend.position = "right", axis.text.x = element_text(angle=45, hjust = 0.5),
          plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

"
- We can see several peaks and dips with regard to the creation of our trending
repositories
- Peaks are at March,2018 and May,2019
- The
general trend of the curve slowly seems to decay and decrease over time after 2019
and that could be because we have a lesser number of trending repositories in 2020 
"

### --- Trending Repo Updated over Time ---
trending_repos_df$updated_monthyear <- format(as.Date(trending_repos_df$updated_at), "%b-%Y")

repos_by_updated_time <- aggregate(trending_repos_df$updated_monthyear,
                                  by=list(trending_repos_df$updated_monthyear), length)
colnames(repos_by_updated_time) <- c("UpdatedTime", "Count")

repos_by_updated_time$UpdatedTime <- mdy(repos_by_updated_time$UpdatedTime)

repos_by_updated_time

# plotting repo vs updated time
ggplot(repos_by_updated_time, aes(x = UpdatedTime, y = Count)) +
    geom_line(aes(color=Count), size=1.5) +
    scale_x_date(date_breaks = "2 week", date_labels = "%B-%Y") +
    geom_text(aes(label=Count), vjust=0.3, position=position_dodge(.9), size=3) +
    labs(x = "Updation Time", y = "Trending Repository Count", 
         title = "Trending Repository vs Updation Time", 
         subtitle = "Total trending repositories last updated in GitHub over time") +
    theme_ipsum_rc(grid="XY", axis_title_just = 0.5)+
    theme(legend.position = "right", axis.text.x = element_text(angle=45, hjust=1),
          plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5))


"
- There is a humongous difference between repo vs creation and repo vs updation

# calculating percentage
tmp <- repos_by_updated_time$Count
tmp[2] / sum(tmp) * 100

- Approx 99.7% of the repo are updated on January 2021
- This adheres to our hypothesis that trending and popular repositories are 
regularly updated over time
"

### --- Repository Metrics ---
"
1. Compute the derived metrics.
2. Aggregate all the necessary metrics by the trending repository owners.
3. Transform the aggregated dataset into an easy to consume format.
4. Analyze and visualize the metric relationships with graphs and correlations.
"

# craeting the ageing vars
trending_repos_df$created_age <- as.integer(difftime(Sys.Date(), 
                                                    trending_repos_df$created_at, 
                                                    units=c("days")))
trending_repos_df$updated_age <- as.integer(difftime(Sys.Date(), 
                                                     trending_repos_df$updated_at,
                                                     units = c("days")))

# get repo owners
trending_repos_df$owner <- sapply(strsplit(trending_repos_df$full_name, "/"), "[", 1)

trending_repos_df %>%
    select(created_age, updated_age, owner)

# aggregate metrics per user
subset_df <- trending_repos_df %>%
    select(id, owner, size, stargazers_count, forks, open_issues, 
           created_age, updated_age,)

stats_df <- sqldf("select owner, count(*) as repo_count, avg(size) as mean_repo_size, 
                  sum(stargazers_count) as total_stargazers, sum(forks) as total_forks,
                  sum(open_issues) as total_open_issues, avg(created_age) as mean_created_age,
                  avg(updated_age) as mean_updated_age from subset_df group by owner 
                  order by repo_count desc")

# transform dataset into attributes & values
corr_df <- stats_df %>%
            select(repo_count, mean_repo_size, total_stargazers, total_forks,
                   total_open_issues, mean_created_age, mean_updated_age)

corr_params <- melt(corr_df)
colnames(corr_params) <- c("Attributes" ,"Value")

# plotting Repo Metric Distributions
ggplot(corr_params, aes(x= Attributes, y = Value, color=Attributes)) +
    geom_boxplot(position = "dodge") + 
    scale_fill_ipsum() +
    labs(x="Metric", y="Value",
         title = "Comparing GitHub repository metric distributions",
         subtitle = "Viewing distributions for various repository metrics") +
    theme_ipsum_rc(grid="Y", axis_title_just = 0.5) +
    scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000)) +
    theme(legend.position = "NA", axis.text.x = element_text(angle=45, hjust=1),
          plot.title = element_text(hjust=0.5), 
          plot.subtitle = element_text(hjust = 0.5))
 
"
- from the graph that the average creation age of repositories are
much higher than the average update age

- The total count of forks is slightly lesser than the total stargazers and that is because
typically more people tend to star repositories than fork them

- The median number
of open issues for repositories is around 50 to 60 based on the plot
"

# plotting Repo Metric Correlations
corrs <- cor(corr_df)

corrplot(corrs, method="number", type="lower", tl.pos = "ld", tl.srt = 45)

"
we can observe very
- strong positive correlations between repo_count and total_stargazers, total_forks and total_open_issues 
- We also see a very strong positive correlation between total_stargazers and total_forks. 
- Besides this, total_open_issues also has strong correlations with repo_count, total_stargazers and total_forks
"

# plotting Stargazers VS repo counts
corr <- format(cor(corr_df$total_stargazers, corr_df$repo_count), digits = 3)

ggplot(corr_df, aes(x=total_stargazers, y = repo_count)) +
    geom_jitter(alpha=1/2)+
    geom_smooth(method=loess) +
    labs(x="Stargazers", y="Repositories",
         title = "Correlation between Stargazers & Total Repositories") +
    annotate("text", label=paste("Corr =", corr), x=+Inf, y=0, hjust=1) +
    theme_ipsum_rc(axis_title_just = 0.5) + theme(plot.title = element_text(hjust=0.5),
                                                  plot.subtitle = element_text(hjust=0.5))
  

"
we can see from the plot that the higher the number
of repositories, the more is the number of stargazers based on the majority of the
data points and the polynomial loess regression line which has been fit
"   

# plotting Stargazers vs fork counts
corr <- format(cor(corr_df$total_stargazers, corr_df$total_forks), digits = 3)

ggplot(corr_df, aes(x=total_stargazers, y = total_forks)) +
    geom_jitter(alpha=1/2)+
    geom_smooth(method=loess) +
    labs(x="Stargazers", y="Total Forks",
         title = "Correlation between Stargazers & Total Forks") +
    annotate("text", label=paste("Corr =", corr), x=+Inf, y=0, hjust=1) +
    theme_ipsum_rc(axis_title_just = 0.5) + theme(plot.title = element_text(hjust=0.5))
                                                  


"
The general trend noticed is that as the count of
stargazers are more in repositories, correspondingly they have more forks.
"
## Build a regression of stragazers and forks
## build a polynomial regression loess model
prm <- loess(total_stargazers ~ total_forks, corr_df)

summary(prm) # polynomial regression model degree : 2

predict(prm, 5000)  # predict total stargazers for 5000 forks

"
so a repo with 5000 forks can have approx. 25,763 stargazers
"

## Relation between total forks and repo count
tmp <- round(max(c(mean(trending_repos_df$open_issues), 
         median(trending_repos_df$open_issues))))
tmp

"
The logic we will be using here is such that if we have 46 or less open issues, we label that
repository as healthy. If the repository has more than 46 open issues, it is labeled as unhealthy
"

# compute repository health
new_corr_df <- copy(corr_df)
new_corr_df$repo_healthy <- ifelse(corr_df$total_open_issues <= tmp, 
                               "Healthy" ,"Not Healthy")

# get correlation coefficient
corr <- format(cor(new_corr_df$total_forks, new_corr_df$repo_count), digits=3)

ggplot(new_corr_df, aes(x=total_forks, y =repo_count, color=repo_healthy)) +
    geom_jitter(alpha=1/2)+
    geom_smooth(method=loess) +
    labs(x="Fork count", y="Repositories",
         title = "Correlation between Total Forks & Total Repositories") +
    annotate("text", label=paste("Corr =", corr), x=+Inf, y=0, hjust=1) +
    theme_ipsum_rc(axis_title_just = 0.5) + 
    theme(plot.title = element_text(hjust=0.5))

## Additional plots
# get correlation coefficient
corr <- format(cor(new_corr_df$total_stargazers, new_corr_df$repo_count), digits=3)

ggplot(new_corr_df, aes(x=total_stargazers, y =repo_count, color=repo_healthy)) +
    geom_jitter(alpha=1/2)+
    geom_smooth(method=loess) +
    labs(x="Stargazers", y="Repositories",
         title = "Correlation between Stargazers & Total Repositories") +
    annotate("text", label=paste("Corr =", corr), x=+Inf, y=0, hjust=1) +
    theme_ipsum_rc(axis_title_just = 0.5) + 
    theme(plot.title = element_text(hjust=0.5))

# get correlation coefficient
corr <- format(cor(new_corr_df$total_open_issues, new_corr_df$repo_count), digits=3)

ggplot(new_corr_df, aes(x=total_open_issues, y =repo_count, color=repo_healthy)) +
    geom_jitter(alpha=1/2)+
    geom_smooth(method=loess) +
    labs(x="Open issues", y="Repositories",
         title = "Correlation between Total Open issues & Total Repositories") +
    annotate("text", label=paste("Corr =", corr), x=+Inf, y=0, hjust=1) +
    theme_ipsum_rc(axis_title_just = 0.5) + 
    theme(plot.title = element_text(hjust=0.5))

# get correlation coefficient
corr <- format(cor(new_corr_df$total_stargazers, new_corr_df$total_open_issues), digits=3)

ggplot(new_corr_df, aes(x=total_stargazers, y =total_open_issues, color=repo_healthy)) +
    geom_jitter(alpha=1/2)+
    geom_smooth(method=loess) +
    labs(x="Stargazers", y="Open Issues",
         title = "Correlation between Stargazers & Total Open Issues") +
    annotate("text", label=paste("Corr =", corr), x=+Inf, y=0, hjust=1) +
    theme_ipsum_rc(axis_title_just = 0.5) + 
    theme(plot.title = element_text(hjust=0.5))





