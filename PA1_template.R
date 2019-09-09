## Loading of Data
df<- read.csv("E:/JHU Data Science/rfiles/Course4/activity.csv", header=TRUE)
head(df)
dim(df)
sum(is.na(df))
str(df)


## What is mean total number of steps taken per day?
library(ggplot2)
total.steps <- tapply(df$steps, df$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)

## What is the average daily activity pattern?
library(ggplot2)
averages <- aggregate(x=list(steps=df$steps), by=list(interval=df$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")

#maximum number of steps?
averages[which.max(averages$steps),]

## how_many_missing
missing <- is.na(df$steps)
# How many missing
table(missing)

# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.data <-df
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
sum(is.na(filled.data))

## --------------------------
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
View(total.steps)
qplot(total.steps, binwidth=1000, xlab="Total number of steps taken each day", ylab = "Frequecy")
mean(total.steps)
median(total.steps)

## ------------------------------------------------------------------------
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)


## ------------------------------------------------------------------------
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")

