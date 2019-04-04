---
output:
  html_document: default
  pdf_document: default
---

Reproducible Research: Week 2 Programming Assignment
====================================================

Loading the csv file and summary
--------------------------------

``` {r}
setwd("C:/Users/Mahe/Documents/R/Rscourse")
library(ggplot2)
unzip("activity.zip")
dataSet<- read.csv("activity.csv")
head(dataSet)
dim(dataSet)
summary(dataSet)
```

1.What is mean total number of steps taken per day?
---------------------------------------------------

### Number of steps per day

``` {r}
SummedDataByDay <- aggregate(dataSet$steps, by=list(dataSet$date), sum)
names(SummedDataByDay)[1] ="date"
names(SummedDataByDay)[2] ="totalsteps"
head(SummedDataByDay,10)
```

### Make a histogram of the total number of steps taken each day

``` {r}
ggplot(SummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "darkgreen", binwidth=1000) +
  labs(title = "Total No. of Steps per Day", x = "Steps", y = "Frequency")
```

### Calculate and report the mean and median of the total number of steps taken per day

``` {r}
mean(SummedDataByDay$totalsteps,na.rm=TRUE)
```

``` {r}
median(SummedDataByDay$totalsteps,na.rm=TRUE)
```

2.What is the average daily activity pattern?
---------------------------------------------

### Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` {r}
dataSetNA <- is.na(dataSet$steps)
cleanBase <- dataSet[!dataSetNA,]

no_NASubset <- dataSet[!dataSetNA,]
MeanDataByInterval <- aggregate(no_NASubset$steps, by=list(no_NASubset$interval), mean)

names(MeanDataByInterval)[1] ="Interval"
names(MeanDataByInterval)[2] ="Steps"

ggplot(MeanDataByInterval, aes(x = Interval, y=Steps)) +
  labs(title = "Sum of Steps every Interval", x = "Interval", y = "Steps")+
  geom_line(color="blue") 
```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` {r}
maxInterval <- MeanDataByInterval[which(MeanDataByInterval$Steps==max(MeanDataByInterval$Steps)),]
maxInterval
```

3.Imputing missing values
-------------------------

### Calculate and report the total number of missing values in the dataset 

``` {r}
missingValues <- sum(dataSetNA)
missingValues
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

### Create a new dataset that is equal to the original dataset with the missing data filled in.

``` {r}
library(magrittr)
library(dplyr)

ReplaceWithMean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
MeanData <- dataSet%>% group_by(interval) %>% mutate(steps= ReplaceWithMean(steps))
head(MeanData)
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

``` {r}
FullSummedDataByDay <- aggregate(MeanData$steps, by=list(MeanData$date), sum)
names(FullSummedDataByDay)[1] ="Date"
names(FullSummedDataByDay)[2] ="TotalSteps"
head(FullSummedDataByDay,15)
```

### Summary of new data : mean & median

``` {r}
summary(FullSummedDataByDay)
```

### Making a histogram

``` {r}
hist(FullSummedDataByDay$TotalSteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20, col="steelblue")
```

### Comparing the mean and median of Old and New data

``` {r}
mean(FullSummedDataByDay$TotalSteps)
median(FullSummedDataByDay$TotalSteps)
```

### Do these values differ from the estimates from the first part of the assignment?

Yes, the mean is the same but the median has risen 1.19 steps.

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

The effect of using mean data per interval as a data impute method for missing values has increased overall data towards the mean.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

``` {r}
library(ggplot2)
MeanData$date <- as.Date(MeanData$date)
MeanData$weekday <- weekdays(MeanData$date)
MeanData$weekend <- ifelse(MeanData$weekday=="Saturday" | MeanData$weekday=="Sunday", "Weekend", "Weekday" )

meandataweekendweekday <- aggregate(MeanData$steps , by= list(MeanData$weekend, MeanData$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
    ggtitle("Comparison of Average Number of Steps in Each Interval")
```
