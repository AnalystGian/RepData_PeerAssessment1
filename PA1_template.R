## Set working directory
dir()
setwd(dir ="D:/DATA ANALYSTS/R/R Course Directory")

## Install necessary packages for loading, pre-processing, and transforming the data.
install.packages("tidyverse")
library(tidyverse)

## Load the data
unzip(zipfile="repdata_data_activity.zip")
data <- read.csv("activity.csv")


## Task 1. What is the mean total number of steps taken per day?
library(ggplot2)
totalsteps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)


## Task 2. What is the average daily activity pattern?
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")


## Maximum number of steps
averages[which.max(averages$steps),]


## Task 3. Imputing missing values
missing <- is.na(data$steps)
# Calculate missing values
table(missing)


##
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)


## Histogram of total number of steps taken each day
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)


## Task 4. Are there differences in activity patterns between weekdays and weekends?
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

# Load necessary packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Convert 'date' to a Date class
filled.data$date <- as.Date(filled.data$date)

# Create a new factor variable 'day_type'
filled.data <- filled.data %>%
  mutate(day_type = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))

# Calculate average steps per interval for weekdays and weekends using dplyr
avg_steps <- filled.data %>%
  group_by(interval, day_type) %>%
  summarize(steps = mean(steps, na.rm = TRUE))

# Load ggplot2 if not already loaded
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Create a panel plot
ggplot(avg_steps, aes(x = interval, y = steps)) +
  geom_line(aes(color = day_type), type = "l", size = 1) +
  facet_grid(day_type ~ ., scales = "free_y") +
  xlab("5-minute interval") +
  ylab("Average number of steps taken") +
  ggtitle("Time Series Plot of Average Steps per 5-Minute Interval") +
  theme_minimal()