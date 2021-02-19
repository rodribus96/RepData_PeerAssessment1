---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
if (!file.exists('activity.csv')) {
  unzip(zipfile = 'activity.zip')
}

activity_data <- read.csv(file = 'activity.csv', header = TRUE)
```

## What is mean total number of steps taken per day?


```r
# Total Steps by day and we plot histogram
steps_day <- aggregate(steps ~ date, activity_data, sum)

hist(steps_day$steps,
     main = "Total Steps by Day",
     xlab = "Number of steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# Calculate mean and median of the total number of steps taken by day
meanSteps <- mean(steps_day$steps, na.rm = TRUE)
medSteps <- median(steps_day$steps, na.rm = TRUE)

print(meanSteps)
```

```
## [1] 10766.19
```

```r
print(medSteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
# Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

library(ggplot2)
meansteps_int <- aggregate(steps ~ interval, activity_data, mean)
ggplot(data = meansteps_int, aes(x = interval, y = steps))+
  geom_line()+
  ggtitle("Mean daily activity pattern")+
  xlab("5 min interval")+
  ylab("Mean number os steps")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxInt <- meansteps_int[which.max(meansteps_int$steps),]
print(maxInt$interval)
```

```
## [1] 835
```

## Imputing missing values


```r
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs

missing_val <- is.na(activity_data$steps)
length(missing_val)
```

```
## [1] 17568
```

```r
# Devise a strategy for filling in all of the missing values in the dataset.
# We the mean for that 5-minute interval
# Create dataset with the imputed data
imp_activitydata <- transform(activity_data,
                              steps = ifelse(is.na(activity_data$steps),
                                             meansteps_int$steps[match(activity_data$interval,
                                                                       meansteps_int$interval)],
                                             activity_data$steps))

# Make a histogram of the total number of steps taken each day and report the mean and median.

imp_stepsbyint <- aggregate(steps ~ date, imp_activitydata, sum)
hist(imp_stepsbyint$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Difference with the other estimates

imp_meanSteps <- mean(imp_stepsbyint$steps, na.rm = TRUE)
imp_medSteps <- median(imp_stepsbyint$steps, na.rm = TRUE)
diffmean <- imp_meanSteps - meanSteps
diffmed <- imp_medSteps - medSteps

print(diffmean)
```

```
## [1] 0
```

```r
print(diffmed)
```

```
## [1] 1.188679
```
## Are there differences in activity patterns between weekdays and weekends?


```r
activity_data$date <- as.POSIXct(activity_data$date) # set the dates to POSIXct

# Add the Weekday/weekend identifier

activity_data$week <- ifelse(weekdays(activity_data$date) == "Saturday" | weekdays(activity_data$date) == "Sunday" ,"weekend","weekday")

#df of the mean and median number of steps taken, averaged across all days (y-axis)
intsteps2 <- aggregate(activity_data$steps, by = list(activity_data$week, activity_data$interval), mean, na.rm=TRUE)
intstepsmed2 <- aggregate(activity_data$steps, by = list(activity_data$week, activity_data$interval), median, na.rm=TRUE)

intsteps2 <- cbind(intsteps2[], intstepsmed2$x)

#Tidy the df names and round the numbers
names(intsteps2) = c("weekday", "interval","mean.steps", "median.steps")
intsteps2$mean.steps <- round(intsteps2$mean.steps)
intsteps2$median.steps <- round(intsteps2$median.steps)


ggplot(intsteps2, aes(x = interval, y = mean.steps)) + ylab("Number of Steps") + geom_line() + facet_grid(weekday~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
