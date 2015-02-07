# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First of all, let's load the data, and check the first and last six rows of the dataset.


```r
data<-read.csv("activity.csv")
head(data)
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
tail(data)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```
We can see that there might be a lot of missing values of the variable "steps" in the dataset.


## What is mean total number of steps taken per day?

Then we need to calculate the total number of steps taken per day. To do so, we need tsplit() functions to make the dataset into different groups by dates, and then calculate the total number of steps taken per day.


```r
data1 <- split(data$steps, data$date)
totalSteps<-sapply(data1, sum,na.rm=TRUE) 
totalSteps
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

Then we can make a histogram of the total number of steps taken each day


```r
dateRange<-seq(as.Date("2012-10-01"), by=1, len=61)
plot(dateRange,totalSteps,type="h",col="red",lwd="7",main="Total Number of Steps Taken Each Day",xlab="Date",ylab="Total Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

We can also calculate and report the mean and median of the total number of steps taken per day.


```r
meanOfTotalSteps<-mean(totalSteps)
meanOfTotalSteps
```

```
## [1] 9354.23
```

```r
medianOfTotalSteps<-median(totalSteps)
medianOfTotalSteps
```

```
## [1] 10395
```


## What is the average daily activity pattern?

Next we'll make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days. To to so, we need to divide the dataset into different groups by time intervals, calculate the mean number of steps in each interval, and then make the plot.


```r
data2 <- split(data$steps, data$interval)
meanSteps<-sapply(data2, mean,na.rm=TRUE)
interval=matrix(data = NA, nrow = 288, ncol = 2)
interval[,1]<-data$interval[1:288]
for (i in 1:288){
    interval[i,1]<-toString(interval[i,1])
}
plot(interval[,1],meanSteps,type="l",col="blue",lwd="2",main="Average Steps in Each Time Interval",xlab="Time Interval",ylab="Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Then we'll write a loop to find the interval with the the maximum number of steps.


```r
max<-meanSteps[1]
for (i in 1:288){
  if (meanSteps[i]>max)
    max<-meanSteps[i]
}
max
```

```
##      835 
## 206.1698
```

We can see that the in interval with the the maximum number of steps is 08:35-08:40.


## Imputing missing values

ext we'll calculate and report the total number of missing values in the dataset

```r
sum(is.na(data))
```

```
## [1] 2304
```

Thus there are 2304 missing values. We can fill in all of the missing values in the dataset by using the mean for that interval. First of all, we need to check whether there is an interval full of missing values.


```r
for (i in 1:288){
 if (sum(is.na(data2[[interval[i,1]]]))==238)
   interval[i,1]
}
```

Since there is no output for the code above, we can conclude that there is no interval full of missing values. Then we can fill in all of the missing values.


```r
data3<-matrix(data = NA, nrow = 288, ncol = 2)
for (i in 1:288){
    interval[i,2]=mean(data2[[interval[i,1]]],na.rm=TRUE)
    data3[i,1]<-as.numeric(interval[i,1])
    data3[i,2]<-as.numeric(interval[i,2])
}
data4<-merge(data,data3,by.x="interval",by.y=1)
for (i in 1:17568){
 if (is.na(data4$steps[i])==1)
   data4$steps[i]=data4$V2[i]
}
data5<-subset(data4,select=c(interval,steps,date))
sum(is.na(data5))
```

```
## [1] 0
```

The output "0" means dataset "data5" no longer has any missing values. 


```r
data6 <- split(data5$steps, data5$date)
totalSteps_1<-sapply(data6, sum,na.rm=TRUE) 
plot(dateRange,totalSteps_1,type="h",col="red",lwd="7",main="Total Number of Steps Taken Each Day",xlab="Date",ylab="Total Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(totalSteps_1)
```

```
## [1] 10766.19
```

```r
medianOfTotalSteps<-median(totalSteps_1)
medianOfTotalSteps
```

```
## [1] 10766.19
```

The mean value changed from 9354.23 to 10766.19, and the meadian changed from 10395 to 10766.19. Imputing missing data made the estimates of the total daily number of steps become bigger.


## Are there differences in activity patterns between weekdays and weekends?

Next let's see whether there are differences in activity patterns between weekdays and weekends. First we'll add a new variable to the dataset indicating whether it is a weekday. Then we divide the dataset into two parts by this variable, and finally see the difference.


```r
data5$day<-"day"
data5$date1<-as.Date(data5$date)
for(i in 1:17568){
  if (weekdays(data5$date1[i])=="Saturday"||weekdays(data5$date1[i])=="Sunday")
      data5$day[i]<-"weekend"
      else
        data5$day[i]<-"weekday"
}
data5$day<-as.factor(data5$day)
data7<-subset(data5,select=c(interval,steps,date1,day))
names(data7)[names(data7)=="date1"] <- "date"
head(data7)
```

```
##   interval    steps       date     day
## 1        0 1.716981 2012-10-01 weekday
## 2        0 0.000000 2012-11-23 weekday
## 3        0 0.000000 2012-10-28 weekend
## 4        0 0.000000 2012-11-06 weekday
## 5        0 0.000000 2012-11-24 weekend
## 6        0 0.000000 2012-11-15 weekday
```

Finally, we'll make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
data_weekday<-subset(data7,day=="weekday",select=c(interval,steps,date,day))
data_weekend<-subset(data7,day=="weekend",select=c(interval,steps,date,day))
data_weekday_1 <- split(data_weekday$steps, data_weekday$interval)
meanSteps_1<-sapply(data_weekday_1, mean,na.rm=TRUE)
data_weekend_1 <- split(data_weekend$steps, data_weekend$interval)
meanSteps_2<-sapply(data_weekend_1, mean,na.rm=TRUE)
par(mfrow = c(2, 1))
plot(interval[,1],meanSteps_1,type="l",col="blue",lwd="2",main="Average Steps in Each Time Interval",xlab="",ylab="weekday")
plot(interval[,1],meanSteps_2,type="l",col="blue",lwd="2",xlab="Time Interval",ylab="weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
