##To make codes readable:
library(knitr)
opts_chunk$set(echo=T)
## return a data.frame
data <- read.csv(unz("desktop/activity.zip", "activity.csv"),header=T, quote="\"", sep=",", colClasses=c("numeric", "Date", "numeric"))
## ignore the missing value
data.nna <- na.omit(data)
## sum up steps per day
daily <- rowsum(data.nna$steps, format(data.nna$date,"%Y-%m-%d"))
## Histogram of the steps per day
daily <- data.frame(daily)
names(daily) <- ("steps")
## Unsolved: margin too large
hist(daily$steps, main = "Daily Steps", xlab = "steps")
## mean and median
mean(daily$steps)
median(daily$steps)
## integrate interval mean steps
library(plyr)
intervalsteps <- ddply(data.nna,~interval, summarise, mean=mean(steps))
## plotting
library(ggplot2)
qplot(data = intervalsteps, geom = "line",x = interval, xlab = "Interval (5 min)", y = mean, ylab = "mean steps", main = "Averaged steps per 5-minutes")
## which interval contains the most steps
intervalsteps[which.max(intervalsteps$mean),]
## Amount of missing values
sum(is.na(data))
## Filling in with 0
data0na <- data
data0na[is.na(data0na)] <- 0
## Plotting daily steps
hist(data0na$steps, main = "Daily steps without missing value", xlab = "steps")
mean(data0na$steps)
median(data0na$steps)
## new factor with two levels: "weekday", "weekend"
data$weekend <- weekdays(data$date) == "Saturday" | weekdays(data$date) == "Sunday"
data$weekend <- factor(data$weekend, levels = c(F, T), labels = c("Weekday", "Weekend"))

activity <- ddply(data, .(interval, weekend), summarize, steps = mean(steps, na.rm = T))

## time series panel plot
library(lattice)
xyplot(steps ~ interval | weekend, activity,type = "l", layout = c(1, 2),main = "Activity Patterns of Weekend and Weekday",xlab = "Interval",ylab = "Steps")
