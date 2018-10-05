---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data



```r
act <- read.csv("activity.csv")
totalEachDay <- tapply(act$steps,act$date,sum,na.rm=TRUE)
```
Variable *totalEachDay* is an array representing the number of steps taken per day. 


## What is mean total number of steps taken per day?

```r
hist(totalEachDay, main="Number of steps taken per day",xlab="Number of steps")
# draw ablines
abline(v=mean(totalEachDay), col="red")
abline(v=median(totalEachDay), col="blue")
legend("topright", legend=c(paste("Mean:",as.character(round(mean(totalEachDay)))), paste(" Median:",as.character(round(median(totalEachDay))   ))),
       col=c("red", "blue"), lty=1:1, cex=0.8)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Answer to the question: The mean total number of steps taken per day (red line) is 9354 while the median (blue line) is 10395. 


## What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)




```r
#compute average steps by interval indices
averageEachInterval <- tapply(act$steps,act$interval,mean,na.rm=TRUE)
plot(names(averageEachInterval), averageEachInterval,type="l", main="Average steps by time interval", xlab="Interval index",ylab="Average  steps")
points(x=names(averageEachInterval)[which.max(averageEachInterval)],y=max(averageEachInterval),col="red")
legend("topright", legend=c(paste("Max index:",names(averageEachInterval)[which.max(averageEachInterval)]), "Max value:", round(max(averageEachInterval))))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Variable *averageEachInterval* represents average steps grouped by index of intervals. 


2.Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?  

Answer to the question: According to *averageEachInterval*, 5-minute interval with index *835* contains the maximum number of steps, which is 206.



## Imputing missing values

1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)  

```r
sum(is.na(act))
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy does not need to be sophisticated. For example, you could use
the mean/median for that day, or the mean for that 5-minute interval, etc.  
To impute the missing date, I use the mean value for corresponding 5-minute interval.


3. Create a new dataset that is equal to the original dataset but with the
missing data filled in.  

```r
#impute missing values using average steps of the same interval index
actnew <- transform(act, steps = ifelse(is.na(steps), as.integer(averageEachInterval[as.factor(interval)]),steps))
```

Each missing value is filled with the mean of number of steps of the corresponding interval index; actnew is the new dataset that is equal to the original dataset but with the
missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. Do
these values differ from the estimates from the first part of the assignment?
What is the impact of imputing missing data on the estimates of the total
daily number of steps?  

```r
totalEachDayNoMissing <- tapply(actnew$steps,actnew$date,sum,na.rm=TRUE)
hist(totalEachDayNoMissing,main="Steps taken per day with imputing mean of the same interval index",xlab="Number of steps")
abline(v=mean(totalEachDayNoMissing), col="red")
abline(v=median(totalEachDayNoMissing), col="blue")
legend("topright", legend=c(paste("Mean:",as.character(round(mean(totalEachDayNoMissing)))), paste(" Median:",as.character(round(median(totalEachDayNoMissing))   ))),
       col=c("red", "blue"), lty=1:1, cex=0.8)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Answer to the question: The derivation between mean and median decreases with imputing missing data. 

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday”
and “weekend” indicating whether a given date is a weekday or weekend
day.  


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#add a factor variable "week" to represent whether the date is a weekday or a weekend day
actweek <- mutate(act, week = factor(case_when(grepl("S(at|un)",weekdays(as.Date(date),abbr=TRUE)) ~"weekday", !grepl("S(at|un)",weekdays(as.Date(date),abbr=TRUE)) ~"weekend")))
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis). 

```r
subsetweekdays <- subset(actweek, actweek$week == "weekday" )
averageEachIntervalWeek <- tapply(subsetweekdays$steps,subsetweekdays$interval,mean,na.rm=TRUE)
subsetweekends <- subset(actweek, actweek$week == "weekend" )
averageEachIntervalWeekend <- tapply(subsetweekends$steps,subsetweekends$interval,mean,na.rm=TRUE)
#draw panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
par(mfrow=c(2,1))
plot(averageEachIntervalWeek,type="l",main="Weekday",xlab="Interval",ylab="Average steps")
plot(averageEachIntervalWeekend,type="l",main="Weekend",xlab="Interval", ylab="Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Answer to the question: From the graph, we can observe that average number of steps in weekdays are more than weekends. 
