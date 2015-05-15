# Load and preprocess the data.
# The dates are stored in the default form YYYY-MM-DD, so we can read the data
# in as the correct classes from the beginning
unzip("activity.zip")
raw_data <- read.csv("activity.csv", header=TRUE, stringsAsFactors=FALSE,
                          colClasses=c("integer", "Date", "integer"))

# Confirm that we read the correct number of dimensions
dim(raw_data)

## What is the mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# So keep only the complete cases.
stepdata <- raw_data[complete.cases(raw_data), ]
#[! is.na(raw_data$steps), c("steps", "date")]
# Calculate the total number of steps taken per day.
totalsteps <- tapply(stepdata$steps, stepdata$date, sum)
    
# Make a histogram of the total number of steps taken each day.
# Played with the breaks until I liked the way it looks.
min(totalsteps)
max(totalsteps)
hist(totalsteps, breaks=seq(0, 25000, by=2500))

# Calculate and report the mean and median of the total number of steps taken per day
mean(totalsteps)
median(totalsteps)

## What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
activity <- as.data.frame(tapply(stepdata$steps, stepdata$interval, mean))
names(activity) <- c("average")
# Now the rownames of activity are the intervals
plot(as.integer(rownames(activity)), activity$average, type='l',
     xlab="Interval", ylab="Average # of steps")

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
which(activity==max(activity))
activity$average[104]

## Imputing missing values

# Note that there are a number of days/intervals where there are 
# missing values (coded as NA). The presence of missing days may 
# introduce bias into some calculations or summaries of the data.

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
# This is the number of rows we read initially minus the number of complete rows.
dim(raw_data)[1]-dim(stepdata)[1]
# Or we could do this
sum(is.na(raw_data$steps))

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.

# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.

# Replace each missing value with the average for that 5-minute interval
# (since we already computed that):
new_data <- raw_data
# add a column which includes the interval average. Need to use rownames.
new_data$interval_av <- sapply(new_data$interval, function(x){activity$average[toString(x)]})
na_indices <- which(is.na(raw_data$steps))
new_data$fixed_steps <- ifelse (is.na(raw_data$steps),
                                new_data$interval_av,
                                raw_data$steps)
                        

# Make a histogram of the total number of steps taken each day and 
# Calculate and report the mean and median total number of steps taken per day. 


new_totalsteps <- tapply(new_data$fixed_steps, new_data$date, sum)

min(new_totalsteps)
max(new_totalsteps)
hist(new_totalsteps, breaks=seq(0, 25000, by=2500))

mean(new_totalsteps)
median(new_totalsteps)

# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total 
# daily number of steps?

# For what dates did we have NA before?
dates_with_na <- unique(new_data$date[which(is.na(new_data$steps))])
# get the data from those dates, and see if all the data on those dates was NA
all(is.na(new_data[new_data$date %in% dates_with_na,]$steps))

# Mean didn't change at all, median changed only slightly.


## Are there differences in activity patterns between weekdays and weekends?

# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels – 
# “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
new_data$typeofday <- ifelse(weekdays(new_data$date) %in% c("Saturday", "Sunday"),
                             "weekend",
                             "weekday")
new_data$typeofday <- factor(new_data$typeofday)

# Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 

# Have to recompute averages
avpertype <- with(new_data, tapply(fixed_steps, 
                                   INDEX=list(interval, typeofday),
                                   FUN=mean))
avpertype <- as.data.frame(avpertype)
avpertype$interval <- rownames(avpertype)   
library(reshape2)
toplot <- melt(avpertype, id=c("interval"))
names(toplot) <- c("interval", "typeofday", "avsteps")
toplot$interval <- as.numeric(toplot$interval)

# See the README file in the GitHub repository to see an example of what this 
# plot should look like using simulated data.
library(lattice)
xyplot(avsteps ~ interval | typeofday, data=toplot, type='l',
       xlab="Interval", ylab="Number of Steps")
