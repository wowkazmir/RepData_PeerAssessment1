---
title: "Reproducible Research Course Project 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
Data <- read.csv("activity.csv")
colnames(Data) <- c("Steps", "Date", "Interval")
Data$Date <- as.Date(Data$Date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
library(dplyr)
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
Total.steps <- summarise(group_by(Data, Date), Steps=sum(Steps))
```


```r
library(ggplot2)
g <- ggplot(Total.steps, aes(x = Steps, binwidth = 500))
g + geom_histogram(breaks= seq(100,24000, by = 3000), col= "black", fill = "steelblue") + labs(x ="Steps",  y = "Frequency") + ggtitle("Total number of steps taken through out the day") + xlim(0, 22500)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

```r
dev.copy(png,'plot1.png', width = 640, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


```r
Mean.steps <- as.integer(mean(Total.steps$Steps, na.rm =TRUE))
Median.steps <- median(Total.steps$Steps, na.rm =TRUE)
```
The mean and median total number of steps taken per day are 10766 and 10765 steps respectively.

## What is the average daily activity pattern?

```r
Mean.steps.interval <- summarise(group_by(Data, Interval), Average=mean(Steps, na.rm=TRUE))

g2 <- ggplot(Mean.steps.interval, aes(x=Interval, y=Average))
g2 + geom_line(type="1", col="steelblue", lwd=1) + labs(x = "Interval", y= "Average number of steps") + 
ggtitle("Average number of steps taken per 5-minute interval across all days")
```

```
## Warning: Ignoring unknown parameters: type
```

![](PA1_template_files/figure-html/interval-1.png)<!-- -->

```r
dev.copy(png,'plot2.png', width = 640, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


```r
max.Steps <- filter(Mean.steps.interval, Average==max(Average))
```

Interval step count 835 contains the maximum number of 206 steps.

## Imputing missing values

```r
Missing <- sum(is.na(Data), na.string = TRUE)
Relative.missing <- Missing/dim(Data) * 100
```

There are a total of 2305 missing values in the dataset. The missing values account for 13.12 percent of the data for the steps variable.

The strategy chosen to account for the missing values in the dataset is to replace them with the average number of steps taken during that 5-minute interval. In this backdrop, if an interval is missing for a specific day the average for that interval for all days is inserted. 


```r
Data2 <- Data

Data2 <- transform(Data2, Steps = ifelse(is.na(Data2$Steps), Mean.steps.interval$Average[match(Data2$Interval, Mean.steps.interval$Interval)], Data2$Steps))
```

While the first day contains no data (NA), a zero has been imputed because using an average interval here would have made the record disproportionately higher than the following day, which only has 126 steps recorded.


```r
Data2[as.character(Data2$Date) == "2012-10-01", 1] <- 0
```

To determine the extent to which the original dataset differs from the new dataset after imputing the missing data, a recalculation of the the steps per day has been made.


```r
New.total.steps <- summarise(group_by(Data2, Date), Steps=sum(as.integer(Steps)))
```


```r
g3 <- ggplot(New.total.steps, aes(x = Steps, binwidth = 500))
g3 + geom_histogram(breaks= seq(100,24000, by = 3000), col= "black", fill = "steelblue") + labs(x ="Steps",  y = "Frequency") + ggtitle("Total number of steps taken through out the day") + xlim(0, 22500)
```

![](PA1_template_files/figure-html/new steps histogram-1.png)<!-- -->

```r
dev.copy(png,'plot3.png', width = 640, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
                             

```r
New.mean.steps <- as.integer(mean(New.total.steps$Steps, na.rm =TRUE))
New.median.steps <- as.integer(median(New.total.steps$Steps, na.rm =TRUE))
```


```r
Mean.diff <- -1 * (New.mean.steps - Mean.steps)
Median.diff <- -1 * (New.median.steps - Median.steps)
```


```r
Total.diff <- sum(New.total.steps$Steps, na.rm =TRUE) - sum(Total.steps$Steps, na.rm =TRUE)
```

* The imputed mean value for the number of steps taken is 10575
* The imputed median value for number of steps taken is 10641
* The difference between the non-imputed and imputed mean values is 191 steps
* The difference between the non-imputed and imputed median values is 124 steps
* The difference between the non-imputed and imputed total number of steps taken per day is 74487.

Imputing the missing data seems to have a substantial impact on the estimates of the total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?


```r
Data2$Weekday <- ifelse(as.factor(weekdays(Data2$Date)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

New.mean.steps.interval <- summarise(group_by(Data2, Interval, Weekday), Average=mean(Steps, na.rm=TRUE))
```


```r
g4 <- ggplot(New.mean.steps.interval, aes(x = Interval, y = Average))
g4 +geom_line(type="1", col="steelblue", lwd=1) + facet_grid(Weekday ~ .) + labs(x = "Interval", y= "Average number of steps") + 
ggtitle("Average number of steps taken per interval during weekdays and weekends")
```

```
## Warning: Ignoring unknown parameters: type
```

![](PA1_template_files/figure-html/panel plot-1.png)<!-- -->

```r
dev.copy(png,'plot4.png', width = 640, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
