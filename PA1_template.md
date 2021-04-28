---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

Nowadays, there are lot of methods to monitor personal health condition. Measuring the personal movement is one of the popular method. By using activity monitoring device, the data of personal movement can be collected and can be analyzed. This data collection is very beneficial in order to improve personal behavior regarding their health.

In this peer review assignment, the activity data has been collected and stored in the course web site. The observation data can obtained as following code,

```r
link <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(link,"./activity_monitoring_data.zip",method = "curl")
```

After the raw data has been downloaded, this data should be loaded and stored under new data frame `data.table`.

```r
unzip("activity_monitoring_data.zip")
data.table <- read.csv("activity.csv")
data.table$date <- as.Date(data.table$date)
```
Under this new data frame, there should be a modification in the type of class in the variable `date`. Originally the type of the data is character, but for the purpose of data processing, then it is modified in to date format.

## What is mean total number of steps taken per day?

For this first question, all the missing values (*NA*) are removed and neglected. This strong assumption is required to simplify data analysis and process. Subsequently, the total number of steps take per day can be determined from this modified data frame.


```r
totalstep.day <- with(na.omit(data.table),
                      aggregate(steps, by=list(date=date), FUN=sum))
```

Moreover, the trend of aggregated number of step per day can be observed by histogram of the total number steps.


```r
with(totalstep.day, hist(x, breaks=10,
                         main ="Histogram of Daily Aggregated Number of Steps",
                         xlab = "Total number of steps taken per day")
     )
```

![](PA1_template_files/figure-html/histogram of total steps per day-1.png)<!-- -->

```r
png(filename = "./figure/Plot1.png", width = 480, height = 480, units = "px")
with(totalstep.day, hist(x, breaks=10,
                         main ="Histogram of Daily Aggregated Number of Steps",
                         xlab = "Total number of steps taken per day")
     )
dev.off()
```

```
## png 
##   2
```

According from the histogram plot, the trend of the steps is quite similar with the normal distribution. Another method to observe the personal behavior regarding the total number of steps per day is by determining its key statistics, in this case mean and median value.


```r
with(totalstep.day, summary(x))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

From the key statistics of the observed data, we can conclude that the aggregated number of steps data has normal distribution due to relatively less margin between its mean and median value. The difference value between mean and median can be neglected since it is lesser than 1%.


## What is the average daily activity pattern?

In this second question, the collected data will be observed on the time basis. Initially all the missing values are removed from the data set. Sequentially, all the data are grouped based on their activity time. Afterward the mean values are calculated from each group. However, in this data processing, there is modification in the type class of variable `Times` (originally is variable `interval`) from *integer* to *time* (from *chron* library). This step is useful for creating time series line plot that represents the trend of daily average steps behavior.

```r
mean.steps <- with(na.omit(data.table),
                       aggregate(steps, by=list(Interval=interval), FUN=mean))

colnames(mean.steps) <- c("Times","Average.steps")

mean.steps$Times <- times(strftime(strptime(paste0(
    mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(mean.steps$Times)),
    mean.steps$Times),
                format="%H%M"),
       format = "%H:%M:%S"))

with(mean.steps, plot(ts(Average.steps, c(0,1), end = c(23,12), frequency = 12),
                      type = "l", main ="Average Daily Activity Pattern",
                      xlab = "Time (Hours)", ylab = "Total number of daily average steps")
     )
```

![](PA1_template_files/figure-html/plot line average step-1.png)<!-- -->

```r
png(filename = "./figure/Plot2.png", width = 480, height = 480, units = "px")
with(mean.steps, plot(ts(Average.steps, c(0,1), end = c(23,12), frequency = 12),
                      type = "l", main ="Average Daily Activity Pattern",
                      xlab = "Time (Hours)", ylab = "Total number of daily average steps")
     )
dev.off()
```

```
## png 
##   2
```

According to the graph, the trend of the daily average steps in the first fifth hour is relatively constant at zero value. Then the trend roses sharply with small fluctuation until it reach a peak in the second fifth hours. After it reached the maximum value, the trend is decreasing and fluctuated until at the last fifth hours of the day. Furthermore, in the last fifth hours of the day, the trend is declining until it reaches zero value.

Moreover, the time series plot provides information that the trend has the highest peak in specific range of time. The highest daily mean of steps can be determined as following, 

```r
index.max <- which.max(mean.steps[,2])
mean.steps[index.max,1]
```

```
## [1] 08:35:00
```


## Imputing missing values

According to the previous analysis, it is shown that the personal behavior trend can be analyzed based on daily and hourly/minutely basis. The trend also can be observed on the histogram or time series plot. Nevertheless, neglecting the missing values may lead to bias in the analysis. In this second part of data exploration, all the missing values will be modified based on it average value on their specific time of activity.

Originally, this data set contains many missing values. In order to prevent this bias, there will be modification in the data set by changing the missing values based on the mean value of steps in 5-minutes intervals. Before conducting the modification, the quantity of missing values have to be analyzed.

```r
with(data.table, sum(is.na(steps)))
```

```
## [1] 2304
```

```r
with(data.table, sum(is.na(steps)))/nrow(data.table)
```

```
## [1] 0.1311475
```
From the calculation, it is shown that the quantity of missing values are 2304, around 13% of the dataset. It can be assumed that the modification with the average value will have minor effect to the key statistics of the original data (mean and median).

Afterwards, the new data frame `new.data.table` is constructed by replicating the original data set. Then, all the missing values in the new data set is exchanged with the daily average steps of their specific time of activity.


```r
new.data.table <- data.table
for (i in seq(nrow(new.data.table))) {
    j <- ceiling(i/nrow(mean.steps)) - 1
    index <- i - j*nrow(mean.steps)
    if (is.na(new.data.table[i,1])){
        new.data.table[i,1] <- mean.steps[index,2]
    }
}
```

After all missing values has been substituted, the new trend can be illustrated by histogram of total number of steps and its key statistics.


```r
new.totalstep.day <- with(new.data.table,
                      aggregate(steps, by=list(date=date), FUN=sum))

with(new.totalstep.day, hist(x, breaks=10,
                         main ="Histogram of Total Number of Steps of Modified Data Set",
                         xlab = "Total number of steps"))
```

![](PA1_template_files/figure-html/new dataset total step per day-1.png)<!-- -->

```r
with(new.totalstep.day, summary(x))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
png(filename = "./figure/Plot3.png", width = 480, height = 480, units = "px")
with(new.totalstep.day, hist(x, breaks=10,
                         main ="Histogram of Total Number of Steps of Modified Data Set",
                         xlab = "Total number of steps")
     )
dev.off()
```

```
## png 
##   2
```

According to this new histogram plot, it is shown that the trend of the modified data set is quite similar with the original data. Also, from the key statistical data, the average value of the new data set is equivalent with the original data. However, there is difference in the median value but since it is quite small, it can be neglected. On the other hand another difference is the IQR value between those data set. The new data set has lesser value of IQR (Inter-Quartile Range) than the original data set.


```r
IQR(totalstep.day$x)
```

```
## [1] 4453
```

```r
IQR(new.totalstep.day$x)
```

```
## [1] 2992
```


## Are there differences in activity patterns between weekdays and weekends?

In this last part of assignment, the modified data frame will be analyzed based on the type of its activity date, whether it is observed in the weekend or weekdays. For this observation, the new variable `Type` is determined based on the date of the activity. This new variable's type should be a factor with 2 level.


```r
for (i in seq(nrow(new.data.table))) {
    date.type <- strftime(new.data.table[i,2], format = "%u")
    date.type <- ifelse (date.type != 7, "weekdays","weekend")
    new.data.table[i,4] <- date.type 
}

colnames(new.data.table)[4] <- "Type"
new.data.table$Type <- as.factor(new.data.table$Type)
```

After the new variable has been constructed, the next step is analyzing the average personal steps behavior with respect to each type of activity date. Initially the new modified data set should be categorized based on its type of activity date and the time of activity. Then, the data format of variable `Times` should be converted into suitable format (*Times). Afterward, the xyplot function is utilized in order to have a plot that categorized based on the factor variable, in this case is variable `Type`.


```r
new.mean.steps <- with(new.data.table,
                      aggregate(steps, by=list(Interval=interval, type = Type), FUN=sum))

colnames(new.mean.steps) <- c("Times","data.type","Average.steps")

new.mean.steps$Times <- times(strftime(strptime(paste0(
    mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(new.mean.steps$Times)),
    new.mean.steps$Times),
                format="%H%M"),
       format = "%H:%M:%S"))

with(new.mean.steps,
     xyplot(Average.steps~Times | data.type,
            type = "l",layout=(c(1,2)),
            main = "New Average Daily Activity Pattern by the Type of the Date",
            xlab = "Time (Hours)", ylab = "Total number of daily average steps")
     )
```

![](PA1_template_files/figure-html/new 5min interval-1.png)<!-- -->

```r
index.max.week <- which.max(new.mean.steps[,3])
new.mean.steps[index.max,1]
```

```
## [1] 08:35:00
```

```r
png(filename = "./figure/Plot4.png", width = 480, height = 480, units = "px")
with(new.mean.steps,
     xyplot(Average.steps~Times | data.type,
            type = "l",layout=(c(1,2)),
            main = "New Average Daily Activity Pattern by the Type of the Date",
            xlab = "Time (Hours)", ylab = "Total number of daily average steps")
     )
dev.off()
```

```
## png 
##   2
```

From the above graphs, it is shown that the activity pattern in the weekend is lower than in the weekdays.


