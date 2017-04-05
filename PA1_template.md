# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity = read.csv("activity.csv", header = TRUE, sep = ",")
activity$date = as.Date(activity$date)
```

## What is mean total number of steps taken per day?
### Calculate the total number of steps taken per day

```r
activity_new = aggregate(steps~date, data = activity, sum)
```

###Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
ggplot(activity_new, aes(x=date, y = steps)) + geom_bar(stat="identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Calculate and report the mean and median of the total number of steps taken per day

```r
mean(activity_new$steps)
```

```
## [1] 10766.19
```

```r
median(activity_new$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
