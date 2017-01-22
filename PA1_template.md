# Reproducible Research: Peer Assessment 1
Moon Choi  
2017 1 22   

## Loading and preprocessing the data
1.Load the data (i.e. read.csv())

```r
rm(list=ls())
library(ggplot2)
unzip(zipfile="activity.zip")
```

```
## Warning in unzip(zipfile = "activity.zip"): error 1 in extracting from zip
## file
```

```r
rawData <- read.csv("activity.csv")
```
2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
tempVal <- vector(length=length(rawData$interval))

for (i in 1:length(rawData$interval)) {
  tempVal[i] <- if (length(rawData$interval[i]) <= 1)
                  paste("000",(rawData$interval[i]))
                 else if(length(rawData$interval[i]) == 2)
                  paste("00",(rawData$interval[i]))
                 else if(length(rawData$interval[i]) == 3)
                  paste("0",(rawData$interval[i]))
                 else
                  rawData$interval[i]
}

tempVal <-gsub(" " ,"", tempVal)

rawData$timeStr <-as.factor(gsub("(\\d{1,2})(\\d{2})", "\\1:\\2", tempVal))
```


## What is mean total number of steps taken per day?
1.Calculate the total number of steps taken per day

```r
stepsEachDay <- aggregate(steps ~ date, rawData, sum, na.rm=TRUE)
```
2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
qplot(stepsEachDay$steps, xlab="total number of steps taken each day", binwidth = 500)
```

![](PA1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
3.Calculate and report the mean and median of the total number of steps taken per day

```r
returnText1st <- paste("Mean Value of steps taken per day is ", mean(stepsEachDay$steps, na.rm=TRUE) ,", and median value is ",median(stepsEachDay$steps, na.rm=TRUE),".")
returnText1st
```

```
## [1] "Mean Value of steps taken per day is  10766.1886792453 , and median value is  10765 ."
```

## What is the average daily activity pattern?
1.Make a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsPer5MinInterval<-aggregate(steps~interval, data=rawData, mean, na.rm=TRUE)

ggplot(data=stepsPer5MinInterval, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") + ylab("Average number of steps taken") 
```

![](PA1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxStep <- stepsPer5MinInterval[which.max(stepsPer5MinInterval$steps),]$interval
returnText2nd <-paste("Maximum number of steps in 5-min interval is",maxStep,".")
returnText2nd
```

```
## [1] "Maximum number of steps in 5-min interval is 835 ."
```

## Imputing missing values
1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)

```r
totlaNaNum <- sum(is.na(rawData$steps))
returnText3rd <-paste("Total number of missing value is ",totlaNaNum,".")
returnText3rd
```

```
## [1] "Total number of missing value is  2304 ."
```
2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
modiDataTemp <- rawData
modiDataTemp$steps[is.na(modiDataTemp$steps)] <- median(rawData$steps, na.rm=TRUE)
```
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
modiData <- modiDataTemp
```
4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
modiStepsEachDay <- aggregate(steps ~ date, modiData, sum, na.rm=TRUE)

qplot(modiStepsEachDay$steps, xlab="total number of steps taken each day", binwidth = 500)
```

![](PA1_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
returnText4th <- paste("Mean Value of steps taken per day is changed", mean(stepsEachDay$steps, na.rm=TRUE) ,"to", mean(modiStepsEachDay$steps, na.rm=TRUE)
                       ,", and median value is changed ",median(stepsEachDay$steps, na.rm=TRUE),"to",median(modiStepsEachDay$steps, na.rm=TRUE),".")
returnText4th
```

```
## [1] "Mean Value of steps taken per day is changed 10766.1886792453 to 9354.22950819672 , and median value is changed  10765 to 10395 ."
```

## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.

```r
modiData$date <- as.Date(modiData$date)
modiData$dayOfTheWeek <- weekdays(modiData$date)
modiData$week <- as.factor(ifelse(modiData$dayOfTheWeek == "Saturday" |
                                       modiData$dayOfTheWeek == "Sunday", "weekend", "weekday"))



weekData <- aggregate(steps ~ interval + week, modiData, mean)
names(weekData) <- c("interval", "dayOTW", "steps")
```
2.Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
xyplot(steps ~ interval | dayOTW, weekData, type = "l", layout = c(1, 2), 
      xlab = "Interval", ylab = "Number of steps")
```

![](PA1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
