---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
library(dplyr)
library(Hmisc)
library(chron)
```


```r
filename<- "activity_monitoring_data.zip"

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, filename, method="curl")
}

if (file.exists("activity_monitoring_data.zip")) { 
  unzip(filename) 
}
```
###Omitting null values


```r
data<- read.csv("activity.csv")
main_data <- na.omit(data)
```


## What is mean total number of steps taken per day?

##### 1. Transforming data

```r
sum_steps<- aggregate(steps ~ date, main_data, FUN=sum)
```

##### 2. Histogram of the total number of steps taken each day

```r
hist(sum_steps$steps,
     col="blue", 
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0, 40),
     main = "Total Number Of Steps Taken Each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

##### 3. Mean & Median total number of steps taken per day

```r
mean_sum_steps <- mean(sum_steps$steps)
median_sum_steps <- median(sum_steps$steps)
```
* Mean: 1.0766189\times 10^{4}
* Median: 10765

## What is the average daily activity pattern?

### Avg Daily Activity Pattern

```r
sum_interval <- aggregate(steps ~ interval, main_data, FUN=sum)
```

#### 1.Plotting line graph using plot() from Base Plotting for Total Steps vs 5-Minute Interval

```r
plot(sum_interval$interval, sum_interval$steps, 
     type = "l", lwd = 2,
     xlab = "Interval", 
     ylab = "Total Steps",
     main = "Total Steps vs. 5-Minute Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### 2.Fetching the 5 min interval which has maximum number of steps

```r
filter(sum_interval, steps==max(steps))
```

```
##   interval steps
## 1      835 10927
```

## Imputing missing values

##### 1. The total number of missing values in the dataset 

```r
table(is.na(data))
```

```
## 
## FALSE  TRUE 
## 50400  2304
```
* Total No of missing values: 2304

##### 2. Creating new dataset with missing values filled in by the mean


```r
data_filled <- data
data_filled$steps <- impute(data$steps, fun=mean)
```

##### 3.Histogram of total number of steps taken each day after imputing missing values


```r
steps_per_day_noNA <- aggregate(data_filled$steps, 
                                by = list(Steps.Date = data_filled$date), 
                                FUN = "sum")
hist(steps_per_day_noNA$x, col = "yellow", 
     main = "Total number of steps taken each day\n(NA replaced by mean)",
     ylim= c(0,40),
     xlab = "Number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

##### 5.Calculating mean and median after imputing average for NA.

```r
sum_steps_impute<- aggregate(steps ~ date, data=data_filled, FUN=sum)
mean_steps_impute <- mean(sum_steps_impute$steps)
median_steps_impute <- median(sum_steps_impute$steps)
```
* Mean: 1.0766189\times 10^{4}
* Median: 1.0766189\times 10^{4}


## Are there differences in activity patterns between weekdays and weekends?

##### 1.Adding new factor variable "dayofweek" indicating whether a given date is a weekday or weekend day

```r
data_filled$dayofweek <- ifelse(is.weekend(data_filled$date), "weekend", "weekday")
```

#### #Number of Weekdays and Weekends

```r
table(data_filled$dayofweek)
```

```
## 
## weekday weekend 
##   12960    4608
```

##### 2. Panel plot containing a time series plot


```r
meaninterval_filled<- aggregate(steps ~ interval + dayofweek, data_filled, FUN=mean)

ggplot(meaninterval_filled, aes(x=interval, y=steps)) + 
  geom_line(color="red", size=1) + 
  facet_wrap(~dayofweek, nrow=2) +
  labs(x="\nInterval", y="\nNumber of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

*As we can see, the number of steps on weekends are higher on average during the day, but the peak is higher on weekdays.

