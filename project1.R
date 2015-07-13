######
# Reproducible Research
# This is the raw working file where script was developed to complete Project 1
#
#######
# Loading and preprocessing the data
# 
# Show any code that is needed to
# 
# 1. Load the data (i.e. read.csv())
if(!exists("activity")){
    activity<-read.csv("activity.csv")
}
# 
# 2. Process/transform the data (if necessary) into a format suitable for your analysis
#seems fine as is


# 
# What is mean total number of steps taken per day?
# 
# For this part of the assignment, you can ignore the missing values in the dataset.
# 
# 1. Calculate the total number of steps taken per day
total_steps_daily <- aggregate(steps ~ date, activity, sum)

# 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
#http://stackoverflow.com/questions/10770698/understanding-dates-and-plotting-a-histogram-with-ggplot2-in-r

#total_steps_daily$date <- as.Date(total_steps_daily$date) # format the date field as a Date

# hist(total_steps_daily$steps, 
#      col=1, 
#      breaks = 53, 
#      main="Distribution of steps per day", 
# #     axes = FALSE,
#      xlab="number of steps", 
#      ylab="number of days",
#      angle = 45)

# 
# 3. Calculate and report the mean and median of the total number of steps taken per day

#mean(total_steps_daily$steps, na.rm = TRUE) # 10766.19
#median(total_steps_daily$steps, na.rm = TRUE) # 10765
#sum(total_steps_daily$steps, na.rm = TRUE) # 570608

# 
# What is the average daily activity pattern?
# 
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# calculate the mean number of steps per interval as a numeric vector
 mean_steps <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
# # calculate the x values
 intervals <- strptime(sprintf("%04d", as.numeric(names(mean_steps))), format="%H%M")
# # plot the mean number of steps per day

# plot(intervals, mean_steps, 
#      type="l", 
#      main="Mean steps per interval across days", 
#      xlab="5 minute intervals", 
#      ylab="Mean steps per interval"
#      )
#abline(v=round(max(mean_steps)), lty=3, col="blue")

# 
# # 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
# max_steps<-which.max(time_series)
# max_steps[1]
# 
# Imputing missing values
# 
# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
# 
# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# originalValue <- complete.cases(activity)  
# missingValues <- length(originalValue[originalValue==FALSE])  # number of records with NA  
# missingValues

# nComplete <- length(originalValue[originalValue==TRUE])   # number of complete records
# title="Missing vs. Complete Cases"  
# barplot(table(originalValue),main=title,xaxt='n')  # render Complete Cases barplot  
# axis(side=1,at=c(.7,1.9),labels=c("Missing","Complete"),tick=FALSE)          # render axis  
# text(.7,0,labels=nMissing, pos=3)                                            # label the NA's bar  
# text(1.9,0,labels=nComplete, pos=3) 
# 
# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

activity2 <- activity                       # copy original df into a new one
for (i in 1:nrow(activity2)){               # loop through the new df
    if (is.na(activity2$steps[i])){         # if a value is missing (NA)
        activity2$steps[i] <- mean_steps[i] # replace it with the value previously
                                            # calculated in mean_steps for the given
    }                                       # interval
}

# 
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
#already did above

# 
# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
total_steps_daily2 <- aggregate(strtoi(steps) ~ date, activity2, sum)
# mean(strtoi(total_steps_daily2$steps), na.rm = TRUE) # 10766.19
# median(total_steps_daily2$steps, na.rm = TRUE) # 10765
# sum(total_steps_daily2$steps, na.rm = TRUE) # 570608
# 
# Are there differences in activity patterns between weekdays and weekends?
# 
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
# 
# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
weekdays(as.Date(total_steps_daily2$date), abbreviate=TRUE)

daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity2$daytype <- as.factor(sapply(activity2$date, daytype))


# 
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
par(mfrow = c(2, 1))

for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = activity2, subset = activity2$daytype == 
                                type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}


