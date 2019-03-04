library(dplyr)
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

newclasses <- c(steps="numeric", date="character",  interval="numeric")
data<-read.csv(filename, header = TRUE, colClasses = newclasses)


data$DateNew <- as.Date(data$date, format="%Y-%m-%d")


##total number of steps taken per day and mean
good<-complete.cases(data)
stepsperday<-group_by(data[good,],DateNew)
summaryStats1<-summarise(stepsperday,stepcount=sum(steps))
max(summaryStats1$stepcount)
hist(summaryStats1$stepcount,
     breaks=seq(from=0, to=22000, by=1000),
     col="red", 
     xlab="Total number of steps", 
     ylim=c(0, 15), 
     main="Histogram of the total number of steps taken each day")

mean(summaryStats1$stepcount)
median(summaryStats1$stepcount)

##the average daily activity pattern
dailypattern<-group_by(data[good,],interval)
summaryStats2<-summarise(dailypattern,intervalstepmean=mean(steps))

plot(summaryStats2$interval, summaryStats2$intervalstepmean, 
     type = "l", 
     xlab = "5-minute interval", 
     ylab = "average number of steps taken")

max_row <- which(summaryStats2$intervalstepmean == max(summaryStats2$intervalstepmean))
max_interval <- summaryStats2[max_row, 1]
max_interval
## Imputing missing values
###1.the total number of rows with NAs
sum(is.na(data$steps))
###3.filling in all of the missing values in the dataset with the average of 5 minute interval
na_row<-which(is.na(data$steps))
summaryStats2_2<-rename(summaryStats2,steps=intervalstepmean)
dataMissingImputed<-merge(data,summaryStats2_2, by='interval')
dataMissingImputed<-arrange(dataMissingImputed,date)
dataMissingImputed2<-mutate(dataMissingImputed[na_row,],steps.x=steps.y)
dataMissingImputedFinal<-rbind(dataMissingImputed2,dataMissingImputed[!(is.na(dataMissingImputed$steps.x)),])
dim(dataMissingImputedFinal)
###4.histogram of the total number of steps taken each day

stepsperday3<-group_by(dataMissingImputedFinal,DateNew)
summaryStats3<-summarise(stepsperday3,stepcount=sum(steps.x))
max(summaryStats3$stepcount)
hist(summaryStats3$stepcount,
     breaks=seq(from=0, to=22000, by=1000),
     col="red", 
     xlab="Total number of steps", 
     ylim=c(0, 15), 
     main="Histogram of the total number of steps taken each day")

mean(summaryStats3$stepcount)
median(summaryStats3$stepcount)

## Are there differences in activity patterns between weekdays and weekends?

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

xyplot(intervalstepmean ~ interval | weekday, summaryStats4, 
              type="l", 
              lwd=1, 
              xlab="Interval", 
              ylab="avg. number of steps", 
              layout=c(1,2))





