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

### --------------------------------------------------------------------------------
## ANALYZING LANGUAGE TRENDS
# aggeregate repos count by language
repo_languages <- aggregate(trending_repos_df$language, 
                        by = list(trending_repos_df$language), length)

colnames(repo_languages) <- c("Languages", "Count")

# get top 25 languages in GitHub
top_languages_counts <- arrange(repo_languages, desc(Count))[1:25,]
top_languages_counts


# plotting Language count
ggplot(top_languages_counts, aes(x=Languages, y=Count, fill=Languages)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Count),
            vjust=-0.3,
            position=position_dodge(0.9), size=3) +
  scale_color_ipsum() +
  labs(x="Language", y="Repository Count",
       title = "Top Trending Languages in GitHub") +
  theme_ipsum_rc(grid="y", axis_title_just = 0.5) +
  theme(legend.position="NA",
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust=0.5))
  
"
From the generated plot, we can see that Python and JavaScript are the clear
winners in the war of trending languages!

"  

# aggregate repo counts by language over time
trending_repos_df$created_year <- format(as.Date(trending_repos_df$created_at), "%Y")

top_languages_by_year <- aggregate(trending_repos_df$language, 
                                    by = list(trending_repos_df$created_year, 
                                              trending_repos_df$language), length)

colnames(top_languages_by_year) <- c("Year", "Language", "Count")

top_languages = arrange(repo_languages, desc(Count))[1:15, c("Languages")]

top_languages_by_year <- top_languages_by_year[top_languages_by_year$Language %in% top_languages,]

# Visualize data
ggplot(top_languages_by_year, aes(x=Language, y=Count, fill=Year)) +
  geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=Count),
            vjust=-0.3,
            position = position_dodge(0.9), size=2.5) +
  scale_color_ipsum() +
  labs(x="Language", y="Repositories Count",
       title="Trending Language in GitHub over time") +
  theme_ipsum_rc(grid="Y", axis_title_just = 0.5) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust=0.5))

"
Every single language had a decrease in the count but onlt Python had a
slight increase
"

## Languages with most open issues
# aggregate mean open issues per language
repo_issues <- aggregate(trending_repos_df$open_issues,
                         by=list(trending_repos_df$language), mean)

colnames(repo_issues) <- c("Language", "Issues")

repo_issues$Issues <- round(repo_issues$Issues, 2)

top_issues_language_counts <- arrange(repo_issues, desc(Issues))[1:25,] 

# Visualize data
ggplot(top_issues_language_counts, aes(x=Language, y=Issues, fill=Language)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Issues),
            vjust=-0.3,
            position=position_dodge(0.9),size=3) +
  scale_color_ipsum() +
  labs(x="Language", y="Issues",
       title = "Language with most open issues on GitHub",
       subtitle = "top language repo with highest mean open issue count") +
  theme_ipsum_rc(axis_title_just = 0.5) +
  theme(legend.position = "NA",
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust=0.5))

"
CodeQl and V language are at the top mean open issues count
"

## Languages with most open issues over time
# aggregate mean issues by language over time
top_issues_language_by_year <- aggregate(trending_repos_df$open_issues,
                                         by = list(trending_repos_df$created_year,
                                                   trending_repos_df$language), mean)

colnames(top_issues_language_by_year) <- c("Year", "Language", "Issues")

top_languages <- arrange(repo_issues, desc(Issues))[1:10, c("Language")]

top_issues_language_by_year <- top_issues_language_by_year[
  top_issues_language_by_year$Language %in% top_languages,]

top_issues_language_by_year$Issues <- round(top_issues_language_by_year$Issues, 2)

# visualize data
ggplot(top_issues_language_by_year, aes(x=Language, y=Issues, fill=Year)) +
  geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=Issues),
            vjust=-0.3,
            posiiton=position_dodge(0.9), size=2) +
  scale_color_ipsum()+
  labs(x="Language", y="Issues", 
       title="Language with most open issues in GitHub oevr time",
       subtitle = "top languages repositories with highest mean open issues over time") +
  theme_ipsum_rc(axis_title_just = 0.5)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=90, hjust=1),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))
"
- Languages like V and System Verilog had more issues in 2019 
- Languages like CodeQl, cuda and Dockerfile had more issues in 2018
"

## Languages with most helpful repo
# here we can use both has_pages and has_wiki
# aggregate helpful repo by language
helpful_repo_language <- aggregate(trending_repos_df$has_wiki, 
                                   by=list(trending_repos_df$language), sum)

colnames(helpful_repo_language) <- c("Language", "Count")

top_helpful_repos <- arrange(helpful_repo_language, desc(Count))[1:25,]

# plot top helpful repos
ggplot(top_helpful_repos, aes(x=Language, y=Count, fill=Language)) +
  geom_bar(stat="identity", position = "dodge")+
  geom_text(aes(label=Count),
            vjust=-0.3,
            position = position_dodge(0.9), size=3) +
  scale_color_ipsum()+
  labs(x="Language", y="Count",
       title="Most helpful repositiories in GitHub by Language")+
  theme_ipsum_rc(axis_title_just = 0.5)+
  theme(legend.position = "NA",
        axis.text.x = element_text(angle = 90, hjust=1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

"
Again Python and JavaScript are the top language-based repo with wikis
"

## Languages with  highest popularity score
# Popularity Score = (2 x forks) + stars
# compute popularity score
trending_repos_df$popularity_score <- ((trending_repos_df$forks * 2) + 
                                      trending_repos_df$stargazers_count)

# aggregate repository popularity scores by language
popular_repo_languages <- aggregate(trending_repos_df$popularity_score,
                                    by=list(trending_repos_df$language), sum)


colnames(popular_repo_languages) <- c("Language", "Popularity")

popular_repo_languages$Popularity <- round(popular_repo_languages$Popularity, 1)

top_popular_repos <- arrange(popular_repo_languages, desc(Popularity))[1:25,]

# plot top popular languages
ggplot(top_popular_repos, aes(x=Language, y=Popularity)) +
  geom_bar(stat="identity", position="dodge", fill="steelblue") +
  geom_text(aes(label=Popularity),
            vjust=-0.3,
            position=position_dodge(0.9), size=2.5) +
  scale_color_ipsum()+
  labs(x="Language", y="Popularity",
       title="Language with most Popularity Score in GitHub",
       subtitle = "top language repositories with highest popularity score") +
  theme_ipsum_rc(axis_title_just = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

"
Python, Javascript and Java are the most popular languages
"  
  
## Language Correlations
# get repository owner and language
trending_repos_df$owner <- sapply(strsplit(trending_repos_df$full_name,"/"), "[", 1)
df <- trending_repos_df[, c("owner", "language")]
df_pivot <- data.table(df)

# create owner-language usage matrix
owner_lang_matrix <- dcast.data.table(df_pivot, owner~language,
                                      fun.aggregate = length,
                                      value.var = "language")

owner_lang_df <-as.data.frame(owner_lang_matrix)

View(owner_lang_df)

# build a language correlation matrix
lang_mat <- owner_lang_df[, 2:length(colnames(owner_lang_df))]
lang_corr <- cor(lang_mat)

# transform language correlations
diag(lang_corr) < NA
lang_corr[upper.tri(lang_corr)] <- NA
lang_corr_final <- melt(lang_corr)

lang_corr_final

# get highly correlated langauges
filtered_corr <- lang_corr_final[complete.cases(lang_corr_final),]
filtered_corr <- filtered_corr[which(filtered_corr$value != 1),]

tmp <- filtered_corr[which(filtered_corr$value >= 0.4), ]
tmp2 <- filtered_corr[which(filtered_corr$value <= -0.03), ]

filtered_corr_final <- rbind(tmp, tmp2)

View(filtered_corr_final)

"
- There are no highly correlated languages
- We can see that Nim and Boo, GLSL and GDScript are quite correlated
- Rust and JavaScript are very slightly negatively correlated
"

###----------------------------------------------------------------------------------------
## ANALYZING USER TRENDS
# get repo user
trending_repos_df$user <- sapply(strsplit(trending_repos_df$full_name, "/"), "[", 1)

# aggregate repository counts by users
repo_users <- aggregate(trending_repos_df$user,
                        by=list(trending_repos_df$user), length)

colnames(repo_users) <- c("User", "Count")

top_users_counts <- arrange(repo_users, desc(Count))[1:25,]

# plot top 25 most contributing users
ggplot(top_users_counts, aes(x=User, y=Count, fill=User)) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip()+
  geom_text(aes(label=Count),
            vjust=0.3,
            hjust=-0.1,
            position=position_dodge(0.9), size=3) +
  scale_color_ipsum() +
  labs(x="User", y="Count",
       title="Top contirbuting user on GitHub",
       subtitle = "Users with the most trending repositories") +
  theme_ipsum_rc(axis_title_just = 0.5) +
  theme(legend.position = "NA",
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust = 0.5))

"
microsoft, google, facebooksearch are top contributing users
"

## ANALYZE USER ACTIVITY METRICS
# compute derived metrics
trending_repos_df$user <- sapply(strsplit(trending_repos_df$full_name, "/"), "[", 1)

trending_repos_df$create_age <- as.integer(difftime(Sys.Date(), 
                                                    trending_repos_df$created_at,
                                                    units=c("days")))
trending_repos_df$update_age <- as.integer(difftime(Sys.Date(), 
                                                    trending_repos_df$updated_at,
                                                    units = c("days")))

# build aggregations
subset_df <- trending_repos_df[c("id", "user", "size","stargazers_count", "forks",
                                 "open_issues", "create_age","update_age")]
stats_df <- sqldf("select user, count(*) as repo_count, 
                  avg(size) as mean_repo_size,
                  sum(stargazers_count) as total_stargazers,
                  sum(forks) as total_forks,
                  sum(open_issues) as total_open_issues,
                  avg(create_age) as mean_create_age,
                  avg(update_age) as mean_update_age 
                  from subset_df group by user
                  order by repo_count desc")

# filter and view stats
top_users_stats <- stats_df[1:20,]
colnames(top_users_stats) <- c("User", "Total_Repos", "Avg.Repo_Size", "Total_Stargazers", "Total_Forks", 
             "Total_Open_Issues", "Avg.Repo_Create_Age", "Avg.Repo_Update_Age")

# scale metric attributes
scale_col <- function(x){
  round((x-min(x)/max(x)-min(x)),2)
}
scaled_stats <- cbind(top_users_stats[,1],
                      as.data.frame(apply(top_users_stats[2:8],
                                          2, scale_col)))

colnames(scaled_stats)[1] <- "User"
scaled_stats_tf <- melt(scaled_stats, id="User") 
colnames(scaled_stats_tf) <- c("User", "Metric", "Value")

# plot user activity metrics
ggplot(data=scaled_stats_tf, aes(x=Metric, y=User)) +
  geom_tile(aes(fill=Value)) +
  geom_text(aes(label=Value), size=3) +
  scale_fill_gradient(low = "#FFB607", high = "#DB3D00") +
  theme_ipsum_rc(axis_title_just = 0.5) +
  labs(x="User", y="Metric",
       title="User Activity Metric Heatmap",
       subtitle="Anlayzing trending user activity metrics on GitHub") +
  theme(legend.position = "NA",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust=0.5))

## adding proportions and Intensity
req_df <- top_users_stats %>%
  group_by("User") 

req_df <- cbind( req_df[1], apply(req_df[2:8], 2, function(x) round(x/max(x), 2)) ) 

scaled_stats_tf <- melt(req_df, id="User")
colnames(scaled_stats_tf) <- c("User", "Metric", "Value")

# plot user activity metrics
ggplot(data=scaled_stats_tf, aes(x=Metric, y=User)) +
  geom_tile(aes(fill=Value)) +
  geom_text(aes(label=Value), size=3) +
  scale_fill_gradient(low = "#FFB607", high = "#DB3D00") +
  theme_ipsum_rc(axis_title_just = 0.5) +
  labs(x="User", y="Metric",
       title="User Activity Metric Heatmap",
       subtitle="Anlayzing trending user activity metrics on GitHub") +
  theme(legend.position = "NA",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust=0.5))

"
- Amoung the top 20 google occurs more time
- Microsoft has highest no. of repos, stargazers, open issues
- United Technology has high proportion of Repo Size
- Secondly, Alibaba and dotnet has more no. of forks and open issues resp.  
- Apart from these Tensor Flow, Tecent, Alibaba also has more repos, forks and stargazers
- The create age ad update age are not less than 0.6 and 0.8 repsectively for all 20
"










