#load necessary packages:
library(ggplot2)

#----------Part 1: Loading and Preprocessing the Data----------
#load the data:
raw <- read.csv("activity.csv")

#change date variable class to date:
raw$date <- as.Date(raw$date)


#----------Part 2: What is the mean total number of steps taken per day?----------
#Calculate total number of steps per day:
totsteps <- aggregate(steps ~ date, data = raw, FUN = "sum")

#Create historgram of total number of steps per day:
png(file = "TotalSteps_Hist1-Raw.png", width = 480, height = 480)
hist(totsteps$steps, xlab = "Total Number of Steps", main = "Histogram of Total Number of Steps per Day (Raw Dataset)")
dev.off()

#Calculate mean and median:
totsteps_mean <- mean(totsteps$steps)
totsteps_med <- median(totsteps$steps)
paste("mean of the total number of steps taken per day: ", totsteps_mean)
paste("median of the total number of steps taken per day: ", totsteps_med)


#----------Part 3: What is the average daily activity pattern?----------
#Calculate average steps by interval for all days:
avsteps <- aggregate(steps ~ interval, data = raw, FUN = "mean")

#Make a time series plot:
png(file = "AvgSteps_TimeSeries1-Raw.png", width = 480, height = 480)
plot(avsteps$interval, avsteps$steps, type = "l", xlab = "Interval (minutes)", ylab = "Average Number of Steps", main = "Average Steps Taken vs. 5 min Interval (Raw Dataset)")
dev.off()


#Calculate maximum and get the interval where it occurs:
avsteps[avsteps$steps == max(avsteps$steps),]$interval


#----------Part 4: Imputing missing values----------
#Calculate total number of missing values:
paste("total number of missing values in raw data: ", sum(is.na(raw)))

#Replace NAs by average for that interval by first merging the two data frames:
df <- merge(raw, avsteps, by = "interval", sort = FALSE)
names(df)[c(2,4)] <- c("steps" , "steps_avg")
df$steps[is.na(df$steps)] <- df$steps_avg[is.na(df$steps)]

#Create new (identical) data set with NA's filled in:
clean <- df[, c(2, 3, 1)]
clean <- clean[order(clean$date),]

#Re-calculate total number of steps per day with clean dataset:
cleantotsteps <- aggregate(steps ~ date, data = clean, FUN = "sum")

#Create historgram of total number of steps per day with clean dataset:
png(file = "TotalSteps_Hist2-Clean.png", width = 480, height = 480)
hist(cleantotsteps$steps, xlab = "Total Number of Steps", main = "Histogram of Total Number of Steps per Day (Cleaned Dataset)")
dev.off()

#Re-calculate mean and median with clean dataset and compare to values from raw dataset:
cleantotsteps_mean <- mean(cleantotsteps$steps)
cleantotsteps_med <- median(cleantotsteps$steps)
paste("mean of the total number of steps taken per day: ", cleantotsteps_mean)
paste("median of the total number of steps taken per day: ", cleantotsteps_med)

diff_mean <- abs(totsteps_mean - cleantotsteps_mean)
diff_med <- abs(totsteps_med - cleantotsteps_med)
diff_clean <- abs(cleantotsteps_med - cleantotsteps_mean)

paste("difference between means before and after imputting data: ", diff_mean)
paste("difference between medians before and after imputting data: ", diff_med)
paste("also, difference between mean and median of clean dataset: ", diff_clean)
paste("it appears that after imputting the averages by interval, the mean and median of the clean data set are equal")

#----------Part 5: Are there differences in activity patterns between weekdays and weekends?----------
#Create new factor variable day, with 2 levels: weekday and weekend:
clean$day <- as.POSIXlt(clean$date)$wday
clean[clean$day > 5,]$day <- "weekend"
clean[clean$day <= 5,]$day <- "weekday"
clean$day <- as.factor(clean$day)

#calculate average steps taken across weekdays and weekends:
avstepsdays <- aggregate(steps ~ interval + day, data = clean, FUN = "mean")

#Create a time series plot of average steps taken for weekdays and weekends as a function of the time interval:
png(file = "AvgSteps_TimeSeries2-Clean.png", width = 480, height = 480)
print(qplot(interval, steps, data = avstepsdays, facets = day ~., geom = "line", main = "Average Steps Taken vs. 5 min Interval (Cleaned Dataset)"))
dev.off()

#Compare overall total amount of steps being taking on weekday's versus weekend's:
tot_wday <- sum(avstepsdays[avstepsdays$day == "weekday", ]$steps)
tot_wend <- sum(avstepsdays[avstepsdays$day == "weekend", ]$steps)
tot_wday - tot_wend