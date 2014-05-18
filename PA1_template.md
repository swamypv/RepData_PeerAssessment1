# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activityData <- read.csv("activity.csv")
head(activityData)
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


## What is mean and median of total number of steps taken per day?
### First Plotting the total number of steps per day against dates

```r
totalStepsByDate <- tapply(activityData$steps, activityData$date, FUN = sum, 
    na.rm = TRUE)
barplot(totalStepsByDate, xlab = "Date", ylab = "Total Number of Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
png(filename = "TotalStepsVsDate")
```

### Now finding the mean and the median of the total number of steps taken per day

```r
averageNumberOfStepsPerDay <- mean(totalStepsByDate)
averageNumberOfStepsPerDay
```

```
## [1] 9354
```

```r
medianNumberOfStepsPerDay <- median(totalStepsByDate)
medianNumberOfStepsPerDay
```

```
## [1] 10395
```


## What is the average daily activity pattern?
### First plotting the average number of steps by period

```r
averageNumberOfStepsPerPeriod <- tapply(activityData$steps, INDEX = activityData$interval, 
    FUN = mean, na.rm = TRUE)

plot(names(averageNumberOfStepsPerPeriod), averageNumberOfStepsPerPeriod, xlab = "Interval", 
    ylab = "Average Number of Steps")

lines(names(averageNumberOfStepsPerPeriod), averageNumberOfStepsPerPeriod)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r

# Now plotting a time series Histogram for a different visualization
barplot(averageNumberOfStepsPerPeriod)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 

### Now calculating the period with the greatest number of steps

```r
periodWithHighestAvgSteps <- names(which.max(averageNumberOfStepsPerPeriod))
periodWithHighestAvgSteps
```

```
## [1] "835"
```


## Imputing missing values

### Calculating the number of records with missing "steps"" values

```r
moOfRecordsWithMissingSteps <- nrow(activityData) - nrow(activityData[complete.cases(activityData), 
    ])
moOfRecordsWithMissingSteps
```

```
## [1] 2304
```

### Now imputing the missing values using the strategy defined as follows:
### The missing "steps" value for a specific period is the average (mean) value of all the "steps" for that particular period

```r
activityDataWithNARemoved <- data.frame()
activityDataWithNAFilled <- data.frame()
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.0.3
```

```r
activityDataWithNaRemoved <- activityData[which(activityData$steps != "NA"), 
    ]
avg_by_interval <- ddply(activityDataWithNaRemoved, .(interval), summarise, 
    steps = mean(steps))
activityDataWithNAFilled <- merge(x = activityData, y = avg_by_interval, by.x = "interval", 
    by.y = "interval", all = TRUE)
activityDataWithNAFilled <- arrange(activityDataWithNAFilled, date)
for (i in 1:nrow(x = activityDataWithNAFilled)) {
    record <- activityDataWithNAFilled[i, ]
    if (is.na(x = record$steps.x)) {
        activityDataWithNAFilled[i, ]$steps.x <- activityDataWithNAFilled[i, 
            ]$steps.y
    }
}
```

### Now creating a histo gram using the data with no NAs

```r
modifiedTotalStepsByDate <- tapply(activityDataWithNAFilled$steps.x, activityDataWithNAFilled$date, 
    FUN = sum)
barplot(modifiedTotalStepsByDate, xlab = "Date", ylab = "Total Number of Steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

### Now calculating the mean and median

```r
averageNumberOfStepsPerDay <- mean(modifiedTotalStepsByDate)
averageNumberOfStepsPerDay
```

```
## [1] 10766
```

```r
medianNumberOfStepsPerDay <- median(modifiedTotalStepsByDate)
medianNumberOfStepsPerDay
```

```
## [1] 10766
```


## Are there differences in activity patterns between weekdays and weekends?
