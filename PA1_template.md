---
title: "Reproducible Research: Peer Assessment 1"
author: "Marisa Souza"
output: html_document
keep_md: true
---

### Loading and preprocessing the data

```r
activity = read.csv("activity.csv", header = TRUE, sep = ",")
activity$date = as.Date(activity$date)
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

### What is mean total number of steps taken per day?
I calculated the total number of steps taken per day

```r
activity_steps = aggregate(steps~date, data = activity, sum)
```

I made a histogram of the total number of steps taken each day

```r
library(ggplot2)
ggplot(activity_steps, aes(x=date, y = steps)) + 
  geom_bar(stat="identity") + 
  labs(x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

I calculated and reported the mean and median of the total number of steps taken per day

```r
mean(activity_steps$steps)
```

```
## [1] 10766.19
```

```r
median(activity_steps$steps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

Now I made a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days


```r
activity_interval = aggregate(steps~interval, data = activity, mean)
plot(activity_interval$interval, activity_interval$steps, type = "l", xlab = "Interval", ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

I calculated the 5-minute interval, on average across all the days in the dataset that contains the maximum number of steps.


```r
activity_interval[activity_interval$steps == max(activity_interval$steps),]$interval
```

```
## [1] 835
```

### Imputing missing values

I calculated and reported the total number of missing values in the dataset.

```r
nrow(activity[is.na(activity)==TRUE,])
```

```
## [1] 2304
```

I invented a strategy for filling in all of the missing values in the dataset. I filled the missing data with the mean of the steps and I created a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_new = activity
activity_new[is.na(activity_new)] = mean(activity_new$steps, na.rm = TRUE)
head(activity_new)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

I made a histogram of the total number of steps taken each day 

```r
activity_new_hist = aggregate(steps~date, data = activity_new, sum)
ggplot(activity_new_hist, aes(x=date, y = steps)) + 
  geom_bar(stat="identity") + 
  labs(x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

I calculated the mean and median total number of steps taken per day but these values now are almost the same after replace missing values.

There isn't impact of imputing missing data on the estimates of the total daily number of steps.


```r
mean(activity_new_hist$steps)
```

```
## [1] 10766.19
```

```r
median(activity_new_hist$steps)
```

```
## [1] 10766.19
```

### Are there differences in activity patterns between weekdays and weekends?

I created a new factor variable in the dataset called week with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

I made a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
activity_new$week = ifelse(weekdays(as.Date(activity_new$date)) %in% c("Saturday","Sunday"), "weekend", "weekday")

activity_new = aggregate(steps~week+interval, data = activity_new, mean)
activity_weekday = activity_new[activity_new$week == "weekday",]
activity_weekend = activity_new[activity_new$week == "weekend",]
par(mfrow = c(2,1), mar = c(4,4,2,1))
plot( activity_weekday$steps~ activity_weekday$interval, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Weekday", ylim = c(0,max(activity_new$steps)))
plot( activity_weekend$steps~ activity_weekend$interval, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Weekend", ylim = c(0,max(activity_new$steps)))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
