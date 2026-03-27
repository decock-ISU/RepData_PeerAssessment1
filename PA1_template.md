---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

### Load the necessary packages into R.


``` r
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
```



## Loading and preprocessing the data
1.Show any code that is needed to load the data (i.e. read.csv()
   

``` r
actDB <- read.csv("activity.csv")
```
2.Show any code that is needed to process/transform the data (if necessary) into a format suitable for your analysis

``` r
#Reformat the time and date
actDB$date <- as.Date(actDB$date, format = "%Y-%m-%d")
actDB$dat <- sprintf("%04d", actDB$interval)
actDB$dat <- format(strptime(actDB$dat, format = "%H%M"), format = "%H:%M:%S")
actDB$dat <- as.POSIXct(paste(actDB$date, actDB$dat), format = "%Y-%m-%d %H:%M:%S")
```

<br>

## What is mean total number of steps taken per day?

1.Calculate the total number of steps taken per day

``` r
totStep <- actDB %>%
  group_by(date) %>%
  summarise(total_sum = sum(steps))
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

``` r
hist(totStep$total_sum, main = "Average Steps Taken", xlab = "Interval")
```

![](PA1_template_files/figure-html/histogram of total-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

``` r
totmean <- mean(totStep$total_sum, na.rm = TRUE)
totmedian <- median(totStep$total_sum, na.rm = TRUE)
```

The mean of the data is **10766.19**.

The median of the data is **10765**.

<br>

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
avgStepint <- actDB %>%
  group_by(interval) %>%
  summarise(intAvg = mean(steps,na.rm = TRUE))

plot(avgStepint$interval, avgStepint$intAvg, 
     main = "Average Steps per Interval",
     xlab = "Time Interval", ylab = "Average Steps",type = "l")
```

![](PA1_template_files/figure-html/interval average-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
maxavg <- max(avgStepint$intAvg)
maxint <- avgStepint$interval[which.max(avgStepint$intAvg)]
```
The highest average steps per interval is **206.17** and it occurs in interval **835**.

<br>

## Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA)

``` r
#determine the number of NA's in the step column
numNA <- sum(is.na(actDB$steps))
```
The number of intervals missing a value is  **2304**.

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in. 

``` r
#substitute in the average for that time interval for any NA's in the original data set 
actDBnoNA <- actDB %>%
  left_join(avgStepint, by = "interval") %>%
  mutate(steps = coalesce(steps, intAvg)) %>%
  select(-intAvg) # Remove the extra column from df_B
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
totStepnoNA <- actDBnoNA %>%
  group_by(date) %>%
  summarise(total_sum = sum(steps))

hist(totStepnoNA$total_sum, main = "Average Steps Taken", xlab = "Interval")
```

![](PA1_template_files/figure-html/hitogram of total-1.png)<!-- -->

``` r
totmeannoNA <- mean(totStepnoNA$total_sum, na.rm = TRUE)
totmediannoNA <- median(totStepnoNA$total_sum, na.rm = TRUE)
```
The mean of the data (after replacing NA's) is **10766.19**.

The median of the data (after replacing NA's) is **10766.19**.

NOTE - mean remain unchanged but median has shifted to match the mean.
<br>

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
actDBnoNA$day_type <- factor(
  (weekdays(actDBnoNA$date) %in% c("Saturday", "Sunday")),
  levels = c(FALSE, TRUE),
  labels = c("weekday", "weekend")
)
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
act_means <- actDBnoNA %>%
  group_by(day_type, interval) %>%
  summarize(
    mean_step = mean(steps, na.rm = TRUE),
    .groups = 'drop' # This drops the grouping structure from the result
  )

ggplot(act_means, aes(x = interval, y = mean_step)) +
  geom_line() +
  labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps")+
  facet_grid(day_type ~.)
```

![](PA1_template_files/figure-html/panel plot-1.png)<!-- -->

