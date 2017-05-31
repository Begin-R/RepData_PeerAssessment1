---
title: "ReproducibleResearchReportActivityDataset"
author: "Harpreet Dang"
date: "April 26, 2017"
output: html_document
---



### Assignment
This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

#### Loading and preprocessing the data

Show any code that is needed to

Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis


```r
# add steps to download and unzip data

fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filename <- "activity.Zip"
download.file(fileurl,filename)
unzip(filename, overwrite = TRUE)


#setwd("C:/Users/dangh/Google Drive/R Code/Working/CourseRa/Reproducible Research/RepData_Activity")
activity <- read.table("activity.csv", sep = ",", header=TRUE, na.strings = "NA")

activity$date <- as.Date(activity$date)

# remove na
actnotnull <- subset(activity, !is.na(activity$steps))
```

#### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
# Total number of steps per day
actsteps <- actnotnull %>% 
            group_by(date) %>% 
            summarise(totsteps = sum(steps))
#histogram on the totalsteps 
qplot(actsteps$totsteps, geom = "histogram", 
        col=I("red"), 
        fill = I("black"), binwidth = 2000, 
        main = "Histogram for Total Steps per Day", 
        xlab = "Total Steps")
```

![plot of chunk CalculateMeanStepsPerDay](figure/CalculateMeanStepsPerDay-1.png)

```r
MeanSteps <- mean(actsteps$totsteps)

MedianSteps <- median(actsteps$totsteps)
```

#### Calculate and report the mean and median of the total number of steps taken per day

Average of steps taken per day is 

```r
MeanSteps 
```

```
## [1] 10766.19
```

Median of steps taken per day is 

```r
MedianSteps 
```

```
## [1] 10765
```




### What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
actavgsteps <- actnotnull %>% 
            group_by(interval) %>% 
            summarise(steps = mean(steps, na.rm = TRUE))


ggplot(data = actavgsteps, aes(x=actavgsteps$interval, y = actavgsteps$steps)) + geom_line()
```

![plot of chunk ComputeAverageDailyActivity](figure/ComputeAverageDailyActivity-1.png)


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
actnotnull[actnotnull$steps == max(actnotnull$steps),]$date
```

```
## [1] "2012-11-27"
```


Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(subset(activity, is.na(activity$steps)))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
actmean <- activity %>% 
            group_by(date) %>% 
            summarise(steps = mean(steps,na.rm = TRUE))

actmean$date <- as.Date(actmean$date)
# for each date the missing values will be replaced by the mean of each date 
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_new_mean <- activity %>% group_by(date) %>% summarise(mean = mean(steps, na.rm = TRUE))
activity_new_mean$mean <- ifelse(is.na(activity_new_mean$mean),0,activity_new_mean$mean)
activity_new_mean$date <- as.Date(activity_new_mean$date)

activity3 <- activity 
activity3[is.na(activity3$steps),]$steps <- activity_new_mean[match(activity3[is.na(activity3$steps),]$date,activity_new_mean$date),]$mean
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# Total number of steps per day
#actmean2 <- ddply(activity_new_mean, .(date), summarize, totsteps = sum(steps))


activity3tot <- activity3 %>% group_by(date) %>% summarise(totsteps = sum(steps))

#histogram on the totalsteps 
qplot(activity3tot$totsteps, geom = "histogram", 
        col=I("red"), 
        fill = I("black"), binwidth = 2000, 
        main = "Histogram for Total Steps per Day", 
        xlab = "Total Steps")
```

![plot of chunk HistogramNewVaues](figure/HistogramNewVaues-1.png)

```r
MeanSteps <- mean(activity3tot$totsteps)

MedianSteps <- median(activity3tot$totsteps)
```


Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
activity3$date <- as.Date(activity3$date)
activity3$weekday <- weekdays(activity3$date)
activity3$weekday <- ifelse(activity3$weekday %in% c("Saturday","Sunday"), "Weekend", "Weekday")
activity3avg <- activity3 %>% group_by(interval, weekday) %>% summarise(mean = mean(steps,na.rm=TRUE))
ggplot(data = activity3avg, aes(interval, mean)) + geom_line() + facet_wrap(~weekday,  nrow=2)
```

![plot of chunk weekdayanalyis](figure/weekdayanalyis-1.png)
 
