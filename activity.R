
# Load the data 

activity_dataset <- "repdata_data_activity.zip"

unzip(activity_dataset)

activity <- read.csv("activity.csv")

str(activity)

## histogram of total steps

library(lubridate)

activity$date <- as.Date(activity$date)
steps_per_day <- aggregate(activity$steps, by=list(activity$date), na.rm=TRUE, sum)

# Assigning column names
colnames(steps_per_day) <- c("date", "total_steps" )
hist(steps_per_day$total_steps, bin=50,breaks = 10,  xlab="Number of steps",ylab="Number of days",  main="Distribution of total steps in each day")


#####

library(dplyr)
# calculate mean and median
steps_per_day_stat <- activity %>%
    group_by(date) %>%
     summarize_all(funs(mean=mean(steps), median=median(steps)))
daily_steps_stat<- steps_per_day_stat[c("date", "steps_mean", "steps_median")]
(daily_steps_stat)

#####
##Time series plot of the average number of steps taken
library(ggplot2)

daily_step_plot <- ggplot(daily_steps_stat, aes(x=date, y=steps_mean)) +
  geom_line() + geom_point()+ 
  xlab("Dates") + ylab("Daily step mean")
daily_step_plot



######
  

the_5min_interval <- aggregate(activity$steps, by=list(activity$interval), na.rm=TRUE, mean)
colnames(the_5min_interval) <- c("interval", "average_steps" )
sub<- the_5min_interval[which(the_5min_interval$average_steps == max(the_5min_interval$average_steps)),]
cat("The interval with Max average steps:", sub$interval)

#####

# dealing with missing data
# Replace NA with the average steps of interval across all days

n <- 61
the_5min_interval1 <- do.call("rbind", replicate(n, the_5min_interval, simplify = FALSE))

# preserving the original data file
activity2<-activity


names(activity2)
names(the_5min_interval1)

activity2$steps[is.na(activity2$steps)] <- the_5min_interval1$average_steps

idx <- is.na(activity2$steps)
activity2$steps[idx] = the_5min_interval1$average_steps

steps_per_day_amputdata <- aggregate(activity1$steps, by=list(activity1$date), na.rm=TRUE, sum)

# Assigning column names
colnames(steps_per_day_amputdata) <- c("date", "total_steps" )
hist(steps_per_day_amputdata$total_steps, bin=50,breaks = 10,  xlab="Number of steps", main="Distribution of total steps in each day")

########
# weekdays vs weekend

activity3<-activity2
activity3$date <- as.Date(activity3$date)

activity4<-activity3%>%
  mutate(day= ifelse(weekdays(activity3$date)=="Saturday" | weekdays(activity3$date)=="Sunday", "Weekend", "Weekday"))

daily_pattern<-activity4 %>%
  group_by(day,interval) %>%
  summarize(daily_steps=mean(steps))


library(lattice)

with(daily_pattern, 
     xyplot(daily_steps ~ interval  | day, type = "p", colors="blue", pch=20,      
            main = "Activity pattern during weekday vs weekend",
            xlab = "Five minutes daily intervals", ylab = "Average number of steps"))



