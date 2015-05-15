---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

### Read in the data set.

After unzipping the file, we read the data in and assign the correct class to each column. The dates are conveniently stored in the default form YYYY-MM-DD so we don't need any special arguments to convert that column to class Date. 


```r
unzip("activity.zip")
raw_data <- read.csv("activity.csv", header=TRUE, stringsAsFactors=FALSE,
                          colClasses=c("integer", "Date", "integer"))
```
Let's see what the data looks like:

```r
head(raw_data)
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

## What is the mean total number of steps taken per day?
We're told that we can ignore the missing values for this part.
It appears that the only column to have missing values is the steps column:

```r
c(any(is.na(raw_data$steps)), any(is.na(raw_data$date)), any(is.na(raw_data$interval)))
```

```
## [1]  TRUE FALSE FALSE
```

For this question, since we're told to ignore missing values, we'll keep only the complete cases:

```r
completedata <- raw_data[complete.cases(raw_data), ]
```
To calculate the total number of steps taken per day we want to take the
sum of the steps column, grouped by date.

```r
totalsteps <- tapply(completedata$steps, completedata$date, sum)
```

### Histogram of total number of steps taken each day.

Here's a histogram of the total number of steps taken each day. I 
played with the "breaks" argument until I liked the histogram.

```r
hist(totalsteps, breaks=seq(0, 25000, by=2500),
     xlab="Total Steps", main="Total Number of Steps Taken Each Day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

### Calculate mean and median of total number of steps taken per day:

```r
mean(totalsteps)
```

```
## [1] 10766.19
```

```r
median(totalsteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### Time series plot of average number of steps taken

We make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis): 


```r
# Compute the average steps, grouped by interval
activity <- as.data.frame(tapply(completedata$steps, completedata$interval, mean))
names(activity) <- c("average")
# Add a column for intervals as integers, so points get plotted in the right order.
activity$interval <- as.integer(rownames(activity))
plot(activity$interval, activity$average, type='l', xlab="Interval", ylab="Average # of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# Find the maximum average steps
maxnum <- max(activity$average) 
# Find the interval(s) that take that value
indices <- which(activity$average==maxnum)
activity$interval[indices]
```

```
## [1] 835
```
It appears to be interval 835, corresponding to the five-minute interval starting at 8:35am.

## Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

To calculate the total number of missing values in the dataset (i.e. the total number of rows with NAs) take the number of rows we read initially minus the number of complete rows:

```r
dim(raw_data)[1]-dim(stepdata)[1]
```

```
## [1] 2304
```
Or, we could directly sum the number of rows where steps is NA. It's a nice sanity check to do the same thing two different ways.

```r
sum(is.na(raw_data$steps))
```

```
## [1] 2304
```

### Strategy for filling in missing values
To fill in the missing values we'll use the mean for that 5-minute interval, since we've already computed those values.

Create a new dataset that is equal to the original dataset but with the  missing data filled in:

```r
new_data <- raw_data
# Add a column which includes the interval average.
new_data$interval_av <- sapply(new_data$interval, 
                               function(x){
                                    index <- which(activity$interval==x)
                                    activity$average[index]
                                })
# Fix the values: If the value of steps is NA replace it with the interval average, 
# otherwise leave it alone.
new_data$fixed_steps <- ifelse (is.na((new_data$steps)), new_data$interval_av, new_data$steps)
```

### Histogram of the total number of steps taken each day, with missing values imputed

Recompute the histogram of the total number of steps taken each day, now that missing values are filled in. It has the same shape as the first histogram, but the frequencies are larger
because we now have more days with step counts.

```r
new_totalsteps <- tapply(new_data$fixed_steps, new_data$date, sum)
hist(new_totalsteps, breaks=seq(0, 25000, by=2500),
     xlab="Total Steps", main="Total Number of Steps Taken Each Day (Missing Values Imputed)")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

Compute the mean and median total number of steps taken per day, now that missing values are filled in:

```r
mean(new_totalsteps)
```

```
## [1] 10766.19
```

```r
median(new_totalsteps)
```

```
## [1] 10766.19
```

The mean is the same as it was before missing values were filled in, while the median has changed slightly.



## Are there differences in activity patterns between weekdays and weekends?

We use the dataset with the filled-in missing values. 

First, create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
# If the day is Saturday or Sunday it's a weekend, otherwise it's a weekeday.
new_data$typeofday <- ifelse(weekdays(new_data$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
new_data$typeofday <- factor(new_data$typeofday)
```

### Panel plot for average number of steps on weekdays vs weekends
Now make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
# Have to recompute averages with values grouped by both interval and whether it's a weekday or weekend.
avpertype <- with(new_data, tapply(fixed_steps, INDEX=list(interval, typeofday), FUN=mean))
avpertype <- as.data.frame(avpertype)
avpertype$interval <- as.integer(rownames(avpertype))
# Reshape the dataframe so that we have a column with type of day 
# that the lattice package can use for grouping data.
library(reshape2)
library(lattice)
toplot <- melt(avpertype, id=c("interval"))
names(toplot) <- c("interval", "typeofday", "avsteps")
class(toplot$interval)
```

```
## [1] "integer"
```

```r
xyplot(avsteps ~ interval | typeofday, data=toplot, type='l', 
       xlab="Interval", ylab="Number of Steps", layout=c(1,2))
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 

This looks reasonable.  On weekends people start taking steps later in the day (maybe they sleep in), and then they take steps throughout the day.  On weekdays there's a big spike around 8 or 9 in the morning, when people would likely be going to work, and then not as many steps in the middle of the day (when they're probably at a desk) as there are on weekends. Of course, in saying this I'm making the assumption that the time intervals correspond to the local time zone of the participants in question.