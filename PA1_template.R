##Get data
if(!file.exists("./data")){dir.create("./data")}
fileurl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
arch<-file.path(getwd(),"./data/repdata-data-activity.zip")	
download.file(fileurl,arch)
unzip("./data/repdata-data-activity.zip",exdir="./data")
setwd("./data")

##table with NA's
activity <- read.csv("activity.csv", sep = ",", header = TRUE)
str(activity)
head(activity)
summary(activity)

##table with out the NA's
activity_na<-activity [with (activity, { !(is.na(steps)) } ), ] 
str(activity_na)
head(activity_na)
summary(activity_na)

##add the data 
activity_steps <- aggregate(steps ~ date, activity_na, sum)

##draw the graph
hist(activity_steps$steps, col=2, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")

##calculate  mean and median of the total number of steps per day
summary(activity_steps)

##to put together the data to draw the time series plot
activity_interval <- aggregate(steps ~ interval, activity, mean)

##create the time series graph
plot(activity_interval$interval, activity_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")


##max average number of steps in an interval
activity_max_stps<-which.max(activity_interval$steps)

activity_interval[activity_max_stps,]

##total row with NA's
sum(is.na(activity))
activity_na<-activity


##to get number of steps each day
activity_na_stps_day<-aggregate(steps ~ date, activity_na, sum)
head(activity_na_stps_day)

##draw the corresponding graph
hist(activity_na_stps_day$steps, main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")

##mean and median of of total number of steps per day
mean(activity_na_stps_day$steps)
median(activity_na_stps_day$steps)

##mean and median of of total number of steps per day without the NA's
mean(activity_steps$steps)
median(activity_steps$steps)


##activity_na$date <- as.Date(activity_na$date, "%Y-%m-%d")
activity_na<-activity
activity_na['date_type']<-weekdays(as.Date(activity_na$date))
activity_na$date_type[activity_na$date_type %in% c('Saturday','Sunday') ] <- "weekend"
activity_na$date_type[activity_na$date_type!= "weekend"] <- "weekday"
activity_na$day_type <- as.factor(activity_na$day_type)

intvl_steps_imputed <- aggregate(steps ~ interval+day_type, activity_na, mean)

##library(ggplot2)

##Draw corresponding graph
qplot(interval, steps, data=intvl_steps_imputed, type='l', geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="") + facet_wrap(~ day_type, ncol=1)
