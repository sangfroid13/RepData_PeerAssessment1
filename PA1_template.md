---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and includes the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

*steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
*date: The date on which the measurement was taken in YYYY-MM-DD format
*interval: Identifier for the 5-minute interval in which measurement was taken



## Loading and preprocessing the data

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.2
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
library(lattice)
##Setting the directory and filename

directory<-"C:/Users/hp/Desktop/Coursera/repdata_data_activity"
filename<-"activity.csv"

setwd(directory)
##Preliminary investigation of data
initial<- read.csv(filename, header = TRUE ,nrows=50)
classes<-sapply(initial, class)
varlist<- colnames(initial)
summary(initial)
```

```
##   steps                 date       interval    
##  Mode:logical   2012-10-01:50   Min.   :  0.0  
##  NA's:50                        1st Qu.:101.2  
##                                 Median :202.5  
##                                 Mean   :186.5  
##                                 3rd Qu.:303.8  
##                                 Max.   :405.0
```

```r
newclasses <- c(steps="numeric", date="character",  interval="numeric")
data<-read.csv(filename, header = TRUE, colClasses = newclasses)

##adding new variable in date format
data$DateNew <- as.Date(data$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

For this part of the assignment, the missing values in the dataset are ignored.

The total number of steps taken per day is calculated,
a histogram of the total number of steps taken each day is made
and mean and median of the total number of steps taken per day is reported

```r
good<-complete.cases(data)
stepsperday<-group_by(data[good,],DateNew)
summaryStats1<-summarise(stepsperday,stepcount=sum(steps))
max(summaryStats1$stepcount)
```

```
## [1] 21194
```

```r
hist(summaryStats1$stepcount,
     breaks=seq(from=0, to=22000, by=1000),
     col="red", 
     xlab="Total number of steps", 
     ylim=c(0, 15), 
     main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean1<- mean(summaryStats1$stepcount)
median1<- median(summaryStats1$stepcount)
mean1
```

```
## [1] 10766.19
```

```r
median1
```

```
## [1] 10765
```


## What is the average daily activity pattern?

For this part,  a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) is made and Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps is shown.


```r
dailypattern<-group_by(data[good,],interval)
summaryStats2<-summarise(dailypattern,intervalstepmean=mean(steps))

plot(summaryStats2$interval, summaryStats2$intervalstepmean, 
     type = "l", 
     xlab = "5-minute interval", 
     ylab = "average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max_row <- which(summaryStats2$intervalstepmean == max(summaryStats2$intervalstepmean))
max_interval <- summaryStats2[max_row, 1]
max_interval
```

```
## # A tibble: 1 x 1
##   interval
##      <dbl>
## 1      835
```


## Imputing missing values

There are a number of days/intervals where there are missing values (NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

For this part,

the total number of missing values in the dataset is calculated and reported;
missing values are then filled as the mean for that 5-minute interval in the dataset;
a new dataset is created that is equal to the original dataset but with the missing data filled in;
a histogram of the total number of steps taken each day is made;
the mean and median total number of steps taken per day is reported
 
 

```r
###1.the total number of rows with NAs
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
###3.filling in all of the missing values in the dataset with the average of 5 minute interval
na_row<-which(is.na(data$steps))
summaryStats2_2<-rename(summaryStats2,steps=intervalstepmean)
dataMissingImputed<-merge(data,summaryStats2_2, by='interval')
dataMissingImputed<-arrange(dataMissingImputed,date)
dataMissingImputed2<-mutate(dataMissingImputed[na_row,],steps.x=steps.y)
dataMissingImputedFinal<-rbind(dataMissingImputed2,dataMissingImputed[!(is.na(dataMissingImputed$steps.x)),])
dim(dataMissingImputedFinal)
```

```
## [1] 17568     5
```

```r
###4.histogram of the total number of steps taken each day

stepsperday3<-group_by(dataMissingImputedFinal,DateNew)
summaryStats3<-summarise(stepsperday3,stepcount=sum(steps.x))
max(summaryStats3$stepcount)
```

```
## [1] 21194
```

```r
hist(summaryStats3$stepcount,
     breaks=seq(from=0, to=22000, by=1000),
     col="red", 
     xlab="Total number of steps", 
     ylim=c(0, 15), 
     main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean2<-mean(summaryStats3$stepcount)
median2<-median(summaryStats3$stepcount)
```
The impact of imputing missing data on the estimates of the total daily number of steps is seen through the difference here:

```r
diffmean<-mean1-mean2
diffmean
```

```
## [1] 0
```

```r
diffmedian<-median1-median2
diffmedian
```

```
## [1] -1.188679
```

## Are there differences in activity patterns between weekdays and weekends?

For this part, 
a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day is created;
a panel plot containing a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis) is made



```r
dataMissingImputedFinal$weekday <- as.factor(
  ifelse(
    weekdays(dataMissingImputedFinal$DateNew) %in% c("Saturday","Sunday"), "Weekend", "Weekday")
  )


weekdaypattern<-group_by(filter(dataMissingImputedFinal,as.character(weekday)=="Weekday"),interval)
weekendpattern<-group_by(filter(dataMissingImputedFinal,as.character(weekday)=="Weekend"),interval)

summaryStats4_1<-summarise(weekdaypattern,intervalstepmean=mean(steps.x))
summaryStats4_1$weekday<-as.factor("Weekday")
summaryStats4_2<-summarise(weekendpattern,intervalstepmean=mean(steps.x))
summaryStats4_2$weekday<-as.factor("Weekend")

summaryStats4<-rbind(summaryStats4_1,summaryStats4_2)

xyplot(intervalstepmean ~ interval | weekday, summaryStats4, 
              type="l", 
              lwd=1, 
              xlab="Interval", 
              ylab="avg. number of steps", 
              layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->








Differences in activity patterns between weekdays and weekends can be observed through the panel plot above.


