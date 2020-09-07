---
title: "Coursera Reproducible Research Project 1"
author: "Yash Punjabi"
output: html_document
---

## Assignment instructions

1. Code for reading in the dataset and/or processing the data
2. Histogram of the total number of steps taken each day
3. Mean and median number of steps taken each day
4. Time series plot of the average number of steps taken
5. The 5-minute interval that, on average, contains the maximum number of steps
6. Code to describe and show a strategy for imputing missing data
7. Histogram of the total number of steps taken each day after missing values are imputed
8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

## Step 1
## Loading the data (read.csv())


```r
setwd("F:/Coding/R Programming/repdata_data_activity/")
activity<-read.csv("activity.csv")
```

Exploring basics of data
 
 ```r
 dim(activity)
 ```
 
 ```
 ## [1] 17568     3
 ```
 
 ```r
 names(activity)
 ```
 
 ```
 ## [1] "steps"    "date"     "interval"
 ```
 
 ```r
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
 
 ```r
 str(activity)
 ```
 
 ```
 ## 'data.frame':	17568 obs. of  3 variables:
 ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
 ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
 ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
 ```
 
 ```r
 #total number of missing data
 sum(is.na(activity$steps))/dim(activity)[[1]]
 ```
 
 ```
 ## [1] 0.1311475
 ```
 
 ```r
 #transforming the date column into date format using lubridate
 library(lubridate)
 activity$date<-ymd(activity$date)
 length(unique(activity$date))
 ```
 
 ```
 ## [1] 61
 ```

## Step 2
## What is mean total number of steps taken per day?


```r
  totalStepsByDay<-aggregate(steps~date, activity, sum)
```

###  Plot
![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

### Mean And Median

```r
mean(totalStepsByDay$steps)
```

```
## [1] 10766.19
```


```r
median(totalStepsByDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

 1.Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

The average number of steps taken:


```r
averageStepsByInterval <- aggregate(steps~interval,activity,mean)
```

Time series plot 

```r
with(averageStepsByInterval,plot(interval,steps,type = 'l'))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averageStepsByInterval[which.max(averageStepsByInterval[,2]),1]
```

```
## [1] 835
```

## Imputing Missing Values
Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.

The total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
missingIndex <- (is.na(activity))
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I decided to fill in all of the missing values in the dataset by the mean number of steps per interval.

Finding the mean number of steps per Interval:

```r
m<-mean(averageStepsByInterval$steps)
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

Imputing missing values with m = 37.3825996


```r
  activity1<-activity
  activity1[missingIndex,1]<-m
  head(activity1)
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

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Finding the total number of steps each day after missing values are imputed and making histogram:


```r
totalStepsByDay1<-aggregate(steps~date, activity1, sum)
hist(totalStepsByDay1$steps, xlab="Class of Total Number of Steps per day", ylab="Number of Days", main="Number of Steps taken each day after missing values are imputed")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

  To calculate the mean and median total number of steps per day, we first find total number of steps   per day


```r
totalStepsByDay1<-aggregate(steps~date, activity1, sum)
mean_afterInput<-mean(totalStepsByDay1$steps)
mean_afterInput
```

```
## [1] 10766.19
```
Mean number of steps taken per day is 1.076618910^{4}.


```r
median_afterInput<-median(totalStepsByDay1$steps)
median_afterInput
```

```
## [1] 10766.19
```
Median number of steps taken per day is 1.076618910^{4}.

Since I imputed the missing values by the mean number of steps per interval, there is no difference in mean before and after imputing that is not surprising. The median has changed a little bit.

## Are there differences in activity patterns between weekdays and weekends?
For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity1$date<-as.Date(activity1$date)
library(dplyr)
```


```r
activity2<-activity1%>%
        mutate(dayType= ifelse(weekdays(activity1$date)=="Saturday" |  weekdays(activity1$date)=="Sunday", "Weekend", "Weekday"))
head(activity2)
```

```
##     steps       date interval dayType
## 1 37.3826 2012-10-01        0 Weekday
## 2 37.3826 2012-10-01        5 Weekday
## 3 37.3826 2012-10-01       10 Weekday
## 4 37.3826 2012-10-01       15 Weekday
## 5 37.3826 2012-10-01       20 Weekday
## 6 37.3826 2012-10-01       25 Weekday
```

2.Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
averageStepByDayTypeAndInterval<-activity2 %>%
  group_by(dayType, interval) %>%
  summarize(averageStepByDay=sum(steps))
```

```
## `summarise()` regrouping output by 'dayType' (override with `.groups` argument)
```

```r
head(averageStepByDayTypeAndInterval)
```

```
## # A tibble: 6 x 3
## # Groups:   dayType [1]
##   dayType interval averageStepByDay
##   <chr>      <int>            <dbl>
## 1 Weekday        0             315.
## 2 Weekday        5             242.
## 3 Weekday       10             231.
## 4 Weekday       15             232.
## 5 Weekday       20             228.
## 6 Weekday       25             283.
```


```r
library(lattice)
with(averageStepByDayTypeAndInterval, 
      xyplot(averageStepByDay ~ interval | dayType, 
      type = "l",      
      main = "Total Number of Steps within Intervals by dayType",
      xlab = "Daily Intervals",
      ylab = "Average Number of Steps"))
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)
