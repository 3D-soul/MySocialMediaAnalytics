library(gh)
library(dplyr)
library(ggplot2)
library(reshape2)
library(hrbrthemes)



## --- basic user info ---
res <- gh("/users/:username", username = "dipanjanS", .limit = Inf)

res
tibble(user_name = res$name, user_login = res$login, 
       user_id = res$id, repos = res$public_repos,
       followers = res$followers, gists = res$public_gists,
       date_created = as.Date(res$created_at),
       date_updated = as.Date(res$updated_at),
       )


## --- get all repos of user ---
res <- gh("/users/:username/repos", username = "dipanjanS", .limit = Inf)

length(res)
vapply(res, "[[", "", "name") # getting all names of repos


"
The Linux repository is owned
by Linus Torvalds, the creator and father of the Linux operating system and also the
person who created Git! Linus himself had stated in 2012 that after the initial days of
intense programming and development of Linux, these days he mostly contributes
by merging code written by other contributors with little programming involvement.
Torvalds still has written approximately 2% of the total Linux kernel and considering
the vast number of contributors, that is still one of the largest percentages of overall
contribution to the Linux kernel
"

### ANALYZING REPO ACTIVITY
## Weekly Commit Frequency 
## --- get all commit activity of a users repo
res <- gh("GET /repos/torvalds/linux/stats/commit_activity", .limit = Inf)
res

total <- c()
weeks <- c()
for (re in res){
    total <- c(total, re$total)
    weeks <- c(weeks,re$week)
}

total
weeks

weeks_conv <- as.POSIXct(weeks, origin="1970-01-01")

commit_activity_df <- data.frame(total ,weeks, weeks_conv)
commit_activity_df <- as_tibble(commit_activity_df)

# ploting the commit activity
ggplot(commit_activity_df, aes(weeks_conv, total)) +
    geom_line(aes(color=total), size=1.5) +
    labs(x="Time", y="Total commits",
         title="Weekly GitHub commit frequency",
         subtitle="Commit history for the linux repository") +
    theme_ipsum_rc(axis_text_size=12, axis_title_just=0.5) + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))


## Commit Frequency Distribution vs Day of the Week
## --- curate daily commit history ---
make_breaks <- function(strt, hour, minute, interval="day", length.out=31) {
    strt <- as.POSIXlt(strt)
    strt <- ISOdatetime(strt$year+1900L, strt$mon+1L,
                        strt$mday, hour=hour, min=minute, sec=0, tz="UTC")
    seq.POSIXt(strt, strt+(1+length.out)*60*60*24, by=interval)
}


all_days <- c()
for (re in res){
    all_days <- c(all_days, unlist(re$days))
}
all_days

days <- rep(c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'), 52)

time_stamp <- make_breaks(min(commit_activity_df$weeks_conv), hour=5, minute=30,
                          interval="day", length.out=362)
time_stamp

commit_history_df <- data.frame(commits=all_days, day=days, time=time_stamp)
commit_history_df <- as_tibble(commit_history_df)
commit_history_df

# plotting commit frequency distribution vs. day of week
ggplot(commit_history_df, aes(x=day, y=commits, color=day)) +
    geom_boxplot(position='dodge') +
    scale_fill_ipsum() +
    labs(x="Day", y="Total commits",
         title="GitHub commit frequency distribution vs. Day of Week",
         subtitle="Commit history for the linux repository") +
    theme_ipsum_rc(grid="Y", axis_text_size=12, axis_title_just=0.5, plot_title_size = 14) + 
    theme(plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5))

## Daily Commit Frequency
# plotting daily commit frequency 
ggplot(commit_history_df, aes(x=time, y=commits, color=day))+
    geom_line(aes(color=day))+ geom_point(aes(shape=day))+
    scale_fill_ipsum() +
    labs(x="Time", y="Total commits",
         title = "Daily GitHub commit frequency",
         subtitle = "Commit history for the linux repository")+
    theme_ipsum_rc(grid="Y", axis_text_size = 12, axis_title_just = 0.5)+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

## Weekly Commit Frequency
# get commit frequency participation
res <- gh("/repos/torvalds/linux/stats/participation", .limit=Inf)
res

commit_freq_df <- data.frame(unlist(res$all), unlist(res$owner))
colnames(commit_freq_df)<- c("all", "owner")
commit_freq_df$contributors <- commit_freq_df$all - commit_freq_df$owner
commit_freq_df$week <- 1:52

commit_freq_df_subset <- commit_freq_df[, c("week", "contributors", "owner")]
commit_freq_df_subset_melted <- melt(commit_freq_df_subset, id="week")

req_df <- tibble(commit_freq_df_subset_melted)
req_df

# plotting weekly commit frequency comparision
ggplot(req_df, aes(x=week, y=value, color=variable))+
    geom_line(aes(color=variable)) + 
    geom_point(aes(shape=variable)) +
    scale_fill_ipsum() +
    labs(x="Week", y="Total commits",
         title = "Weekly GitHub commit frequency comparision",
         subtitle = "Commit history for the linux repository") +
    theme_ipsum_rc(grid="Y", axis_title_just = 0.5, plot_title_size = 14) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

" NOTE: over the past year, the frequency of commits made by
contributors is really high as compared to that of the owner."


## Weekly Code Modification History
# get the code frequency data
res <- gh("/repos/torvalds/linux/stats/code_frequency", .limit = Inf)
res

time <- c()
additions <- c()
deletions <- c()
for (re in res){
    time <- c(time, re[[1]])
    additions <- c(additions, re[[2]])
    deletions <- c(deletions, abs(re[[3]]))
}

code_freq_df <- data.frame(time, additions, deletions)
code_freq_df$time <- as.Date(as.POSIXct(code_freq_df$time, origin="1970-01-01"))
code_freq_df_melted <- melt(code_freq_df, id="time")
req_df <- as_tibble(code_freq_df_melted)

# plotting the code frequency timeline
ggplot(req_df, aes(x=time, y=value, color=variable))+
    geom_line(aes(color=variable))+
    geom_point(aes(shape=variable))+
    scale_x_date(date_breaks = "12 month", date_labels="%Y")+
    scale_y_log10(breaks=c(10, 100, 1000, 10000, 100000, 1000000))+
    labs(x="Time", y="Total code modification(log)", 
         title = "Weekly GitHub code frequency timeline",
         subtitle = "Code frequency history for the linux repository") +
    theme_ipsum_rc(grid="XY", axis_title_just = 0.5,axis_text_size = 10)+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

"
The plotted graph portrays an interesting picture. The obvious fact is that the total
additions to the code are usually more than the total deletions. But we can see
that the code additions and deletions start around 2000-01 when GNOME and
KNOPPIX, two major Linux distributions, were released. We also notice a steep
increase in the code modification frequency around 2004-05, which is around the
time Ubuntu, one of the most popular Linux distributions, came into existence!
"

