#setwd before running code
data1 <- "activity.zip"

#download the file
if(!file.exists(data1)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileURL, data1, method = "curl")
}
#unzip the file
if(!file.exists("activity.csv")){
        unzip(data1)
}
#read in the data
activity <- read.csv("activity.csv")
#convert variable date to class "Date"
activity$date <- as.Date(activity$date)
#create subset of data that omits NAs. Also, group by day and find total steps per day
library(dplyr)
activity_subset <- activity %>%
        filter(steps != "NA") %>%
        group_by(date) %>%
        summarise(Steps = sum(steps))
#histogram of total steps by day
hist(activity_subset$Steps, xlab = "Number of Steps per Day",
        main = "Histogram of Total Steps per Day", col = "blue", labels = T, ylim = c(0,30))
#calculate the mean and median number of steps taken each day
mean_steps = mean(activity_subset$Steps)
median_steps = median(activity_subset$Steps)
#report each
print(mean_steps)
print(median_steps)
#group by interval and find average of each interval
activity_interval_subset <- activity %>%
        filter(steps != "NA") %>%
        group_by(interval) %>%
        summarise(steps = mean(steps))
#find the interval that had the maximum value of average steps
max_steps <- filter(activity_interval_subset, steps == max(steps))
#create time series of interval averages
library(ggplot2)
ggplot(activity_interval_subset, aes(interval, steps)) +
        geom_line() +
        geom_text(data = max_steps, aes(x = interval, y = steps, label = "Max = Interval 835")) +
        ggtitle("Average Steps for Each Five Minute Interval")
#calculate the number of rows with NA values for steps (2304)
sum(is.na(activity$steps))
#use mean of each interval and apply this mean to missing values that correspond to that interval
#create data frame with just the NA rows
activity1 <- activity[rowSums(is.na(activity)) > 0, ]
activity1$steps <- as.numeric(activity1$steps)
activity_interval_subset <- as.data.frame(activity_interval_subset)

library(DataCombine)
filled_data <- FillIn(activity1, activity_interval_subset, "steps", "steps", "interval")
#create subset of activity data that omits NAs
activity_subset_NA.RM <- activity %>%
        filter(steps != "NA")

#merge the data
merged_data <- (rbind(activity_subset_NA.RM, filled_data))

#group new data set by day
grouped_merged <- merged_data %>%
        group_by(date) %>%
        summarise(Steps = sum(steps))
#create histogram
hist(grouped_merged$Steps, xlab = "Number of Steps per Day",
        main = "Histogram of Total Steps per Day", col = "red", labels = T, ylim = c(0,40))

#create new factor indicating if the day is weekday or weekend
merged_data$day <- ifelse(weekdays(merged_data$date) %in% c("Saturday", "Sunday"), "weekend",
        "weekday")
merged_days <- merged_data %>%
        group_by(interval, day) %>%
        summarise(Steps = mean(steps))

ggplot(merged_days, aes(interval, Steps)) +
        geom_line() +
        ggtitle("Average Steps for Each Five Minute Interval on Weekends") + 
        facet_wrap(~day)
  


