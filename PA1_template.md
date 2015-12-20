# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?



```r
totalNumberOFSteps <- tapply(data$steps, data$date, sum, na.rm=TRUE)
hist(totalNumberOFSteps, xlab="total number of steps taken each day")
```

![](PA1_template_files/figure-html/StepsEachDay-1.png) 

### mean


```r
mean(totalNumberOFSteps, na.rm=TRUE)
```

```
## [1] 9354.23
```

### median


```r
median(totalNumberOFSteps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
  Here is the average number of steps by 5-minute interval.


```r
averages <- tapply(data$steps, data$interval, mean, na.rm=TRUE)

plot (names(averages), averages, type="l",
      main="Plot of the average number of steps",
      xlab="5-minute interval",
      ylab="average number of steps")
```

![](PA1_template_files/figure-html/PlotOfAverageStteps-1.png) 

The 5-minute interval which contains maximum number of steps is as
follows.


```r
as.numeric(names(which.max(averages)))
```

```
## [1] 835
```

## Imputing missing values

The total number of missing values in the dataset is,


```r
sum(is.na(data))
```

```
## [1] 2304
```

The strategy for filling in all of the missing values in the dataset
is,

 - use the mean for that 5-minute interval.

By using this strategy, I crate a new dataset with the missing data
filled in.


```r
dataNoNA <- data

for (i in which(is.na(dataNoNA))) {
    dataNoNA[i,1] <- averages[((i-1)%%288)+1]
}
```

### New Histogram

  This is a new  histogram that shows total number of steps taken each day.


```r
total.steps <- tapply(dataNoNA$steps, dataNoNA$date, sum, na.rm=TRUE)
hist(total.steps, xlab="total number of steps taken each day")
```

![](PA1_template_files/figure-html/StepsEachDayNoNA-1.png) 

### mean


```r
mean(total.steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

### median


```r
median(total.steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

### analysis
- The value for mean in my new file is not the same in as the original.
- The median for my new file is the same as the original.

## Are there differences in activity patterns between weekdays and weekends?

weekdays <- weekdays(ds_filled_na$date)
ds_filled_na$weekdays <- ifelse(weekdays == "sunday" | weekdays == "saturday", 
    "Weekend", "Weekday")

steps_per_week <- aggregate(ds_filled_na$steps, by = list(ds_filled_na$interval, 
    ds_filled_na$weekdays), mean)

names(steps_per_week) <- c("interval", "weekdays", "steps")
library(lattice)
xyplot(steps ~ interval | weekdays, steps_per_week, type = "l", layout = c(1, 
    2), xlab = "Interval", ylab = "Number of steps", main = "Activity Patterns on Weekends and Weekdays", 
    col = "steelblue")
