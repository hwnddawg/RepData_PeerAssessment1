---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


# Reproducible Research: Peer Assessment 1  


## Data 

The data for this assignment can be downloaded from the course web site:

  - Dataset [Activity monitoring data] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are

  - **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

  - **date**: The date on which the measurement was taken in YYYY-MM-DD format

  - **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Assignment

### Set knitr options


```r
opts_chunk$set(echo = TRUE, results = "markup", warning = FALSE, cache = TRUE, tidy = TRUE)
```

### Loading and preprocessing the data

1. Load the data (i.e. read.csv())


```r
activity <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date <- as.Date(activity$date)
```

### What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
sumperday <- aggregate(activity$steps, by = list(activity$date), sum, na.rm = TRUE)
colnames(sumperday) <- c("Date", "Steps")
plot(sumperday, type = "h", main = "Total Steps per Day")
```

![plot of chunk HistSumPerDay](figure/HistSumPerDay-1.png) 
  
*Note: steps per day are summed with na.rm = TRUE*


2. Calculate and report the mean and median total number of steps taken per day


```r
mean(sumperday$Steps, na.rm = TRUE)
```

```
## [1] 9354.23
```

*The mean total number of steps per day is*  **9,354.23**


```r
median(sumperday$Steps, na.rm = TRUE)
```

```
## [1] 10395
```

*The median total number of steps per day is*  **10,395**

### What is the average daily activity pattern?


1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
meanperint <- aggregate(activity$steps, by = list(activity$interval), mean, 
    na.rm = TRUE)
colnames(meanperint) <- c("Interval", "Steps")
plot(meanperint, type = "l", main = "Average Number of Steps per Interval")
```

![plot of chunk LineMeanPerInt](figure/LineMeanPerInt-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxsteps <- max(meanperint$Steps)
meanperint[meanperint$Steps == maxsteps, ]
```

```
##     Interval    Steps
## 104      835 206.1698
```
*The 5-minute interval that contains the most number of steps is Interval#*  **835**

### Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
*The total number of missing values is* **2,304**

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

*The strategy will be to use the average (across all days) of steps for that interval as the imputed value.* 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
actfill <- activity

for (i in 1:nrow(actfill)) {
    if (is.na(actfill[i, "steps"])) 
        actfill[i, "steps"] <- as.integer(meanperint$Steps[meanperint$Interval == 
            actfill[i, "interval"]])
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
sumperday2 <- aggregate(actfill$steps, by = list(actfill$date), sum, na.rm = TRUE)
colnames(sumperday2) <- c("Date", "Steps")
plot(sumperday2, type = "h", main = "Total Steps per Day (Imputed)")
```

![plot of chunk HistSumPerDayImputed](figure/HistSumPerDayImputed-1.png) 


```r
mean(sumperday2$Steps, na.rm = TRUE)
```

```
## [1] 10749.77
```

*The mean total number of steps (imputed NAs) per day is*  **10,749.77** 



```r
median(sumperday2$Steps, na.rm = TRUE)
```

```
## [1] 10641
```

*The median total number of steps (imputed NAs) per day is*  **10,641** 


Do these values differ from the estimates from the first part of the assignment? 
*Yes!*

What is the impact of imputing missing data on the estimates of the total daily number of steps?  
*Due to the imputed NA values, the computed mean and median are higher and the plots are smoother.*



### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
daytype <- character()

for (i in 1:nrow(actfill)) {
    if (weekdays(actfill[i, "date"]) %in% c("Saturday", "Sunday")) {
        daytype <- c(daytype, "Weekend")
    } else {
        daytype <- c(daytype, "Weekday")
    }
}

actday <- cbind(actfill, daytype)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#  See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
actweekdays <- actday[daytype == "Weekday", ]
weekdaymean <- aggregate(actweekdays$steps, by = list(actweekdays$interval), 
    mean, na.rm = TRUE)
colnames(weekdaymean) <- c("Interval", "Steps")

actweekends <- actday[daytype == "Weekend", ]
weekendmean <- aggregate(actweekends$steps, by = list(actweekends$interval), 
    mean, na.rm = TRUE)
colnames(weekendmean) <- c("Interval", "Steps")

par(mfrow = c(2, 1))
plot(weekdaymean, type = "l", main = "Average Number of Steps per Interval - Weekdays")
plot(weekendmean, type = "l", main = "Average Number of Steps per Interval - Weekends")
```

![plot of chunk PanelPlotWeekdaysvsWeekends](figure/PanelPlotWeekdaysvsWeekends-1.png) 

*Yes, clearly there are differences in activity patterns between weekdays and weekends.*
