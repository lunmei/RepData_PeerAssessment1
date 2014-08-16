# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activityData <- read.csv("activity.csv")
```


## What is mean and median total number of steps taken per day?

```r
stepsPerDay <- tapply(activityData[,1], activityData[,2], sum)
meanStepsPerDay<- mean( stepsPerDay, na.rm = TRUE)
meanStepsPerDay
```

```
## [1] 10766
```

```r
medianStepsPerDay<- median( stepsPerDay, na.rm = TRUE)
medianStepsPerDay
```

```
## [1] 10765
```


```r
hist(stepsPerDay, breaks = 200)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

## What is the average daily activity pattern?

```r
stepsPattern <- tapply(activityData[,1], activityData[,3], mean, na.rm = TRUE)
timeInterval <- activityData[1:length(stepsPattern),3]
plot(timeInterval, stepsPattern, type ="l", 
     xlab ="time interval", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


## Imputing missing values

```r
NumMissingValue <- sum(is.na(activityData[,1]))
print(c("Number of Missing Value: ", NumMissingValue))
```

```
## [1] "Number of Missing Value: " "2304"
```

```r
activityDataFilledMissing <- activityData
activityDataFilledMissing[is.na(activityData[,1]), 1] <- rep(stepsPattern, NumMissingValue/length(stepsPattern))

stepsPerDay2 <- tapply(activityDataFilledMissing[,1], activityData[,2], sum)
meanStepsPerDay2<- mean( stepsPerDay2)
meanStepsPerDay2
```

```
## [1] 10766
```

```r
medianStepsPerDay2<- median( stepsPerDay2)
medianStepsPerDay2
```

```
## [1] 10766
```

```r
hist(stepsPerDay2, breaks = 200)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

I replace the missing value by the average/mean steps for that 5 minute interval. The mean value does not change, the median value changes. After imputing missing data, the estimates of the toal daily number of steps deos not change too much. 

## Are there differences in activity patterns between weekdays and weekends?

```r
activityDataFilledMissing[,2] <- as.Date(activityDataFilledMissing[,2])
weekdaysData <- weekdays(activityDataFilledMissing[,2]) 
weekdaysData[weekdaysData == "Sunday" | weekdaysData =="Saturday"] <- "weekend"
weekdaysData[! weekdaysData == "weekend"] <- "weekday"
factorWeekDays <-factor(weekdaysData)
activityDataFilledMissing[,4] <- factorWeekDays

listtemp <- split(activityDataFilledMissing, activityDataFilledMissing[,4])
stepsPatternWeekdays <- tapply(listtemp$"weekday"[,1], listtemp$"weekday"[,3], mean)
stepsPatternWeekend  <- tapply(listtemp$"weekend"[,1], listtemp$"weekend"[,3], mean)

stepsPatternDay <-data.frame(steps = c(stepsPatternWeekdays, stepsPatternWeekend),
                             day = c(rep("weekday", 288), rep("weekend", 288)), 
                             Interval = rep(timeInterval, 2))
library(lattice)
xyplot(steps ~ Interval | day, data = stepsPatternDay, layout = c(1:2), 
       ylab = "Number of Steps", xlab = "Interval", type ="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 
