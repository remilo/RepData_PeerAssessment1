# Reproducible Research: Peer Assessment 1

#Defining the date format to US (function run for unix system only). Have to be replaced by the appropriate definition for windows system

```r
Sys.setlocale("LC_TIME", "en_US.UTF-8")
```

```
## [1] "en_US.UTF-8"
```





## Loading and preprocessing the data


```r
#load the requested library
library(dplyr)
library(data.table)
library(ggplot2)

#load the data
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?


```r
#remove NA data and prepare the table
table <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)
```

1- Calculate the total number of steps taken per day

```r
table
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```


2- If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(table$steps,
     col="blue",
     main="Total number of steps taken each day",
     xlab="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



3- Calculate and report the mean and median of the total number of steps taken per day


The mean is:

```r
mean(table$steps)
```

```
## [1] 10766.19
```


## What is the average daily activity pattern?

1- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
table_5min <- aggregate(steps ~interval, data=data, mean, na.rm=TRUE)
plot(steps ~ interval, data=table_5min, type="l", main="time series plot of 5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


2- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
table_5min[which.max(table_5min$steps), "interval"]
```

```
## [1] 835
```


## Imputing missing values
1- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I decided to fill in the data with the mean for that 5-minute interval

3- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data_withoutNA <- data
for (i in row(data_withoutNA)){
    if (is.na(data_withoutNA[i,"steps"])){
        data_withoutNA[i,"steps"] <- table_5min[which(table_5min$interval==data_withoutNA[i,"interval"]),"steps"]
    }
}
```

4- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
#remove NA data and prepare the table
table_steps_withoutNA <- aggregate(steps ~ date, data = data_withoutNA, sum, na.rm = TRUE)
#display the graph
hist(table_steps_withoutNA$steps,
     col="blue",
     main="Total number of steps taken each day (after replacing NA)",
     xlab="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

The mean with NA value replaced is

```r
mean(table_steps_withoutNA$steps)
```

```
## [1] 10766.19
```
Whereas the value from part 1 was

```r
mean(table$steps)
```

```
## [1] 10766.19
```
The median with NA value replaced is

```r
median(table_steps_withoutNA$steps)
```

```
## [1] 10766.19
```
Whereas the value from part 1 was

```r
median(table$steps)
```

```
## [1] 10765
```

The mean value was not really changed by replacing the missing value with my strategy. However the median increased from 10765 to 10766.19. In addition we can see that by replacing the missing value, the median and the mean are now both equal to 10766.19.



## Are there differences in activity patterns between weekdays and weekends?

1- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
#add column with week day
data_withoutNA$day <- weekdays(as.Date(data_withoutNA$date, "%Y-%m-%d"))

#create function to define if weekend or not
weekend = function(x){
    if (x %in% c("Saturday", "Sunday")){
        return("weekend")
    }
    
    return("weekday")
}

#create the factor variable with the weekday and weekend value 
data_withoutNA$dayType <- as.factor(sapply(data_withoutNA$day, weekend))
```

2- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
data_withoutNA <- group_by(data_withoutNA, interval, dayType)
table_5min_without_NA <- summarise(data_withoutNA,
                                   steps=mean(steps))


#display the graph
library(lattice)
xyplot(steps~interval | dayType, data = table_5min_without_NA,
      type = 'l',
      xlab = 'Interval',
      ylab = 'Number of Steps',
      main = 'Time series plot of the 5-minute interval', 
      layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


