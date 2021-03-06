
# Reproducible Research: Peer Assessment 1


## Load and process the data.
##### 1. Load the data. 

```{r, echo=FALSE, results='hide', warning=FALSE, message=FALSE}
library(ggplot2)
library(scales)
library(Hmisc)
```

```{r, results='markup', warning=TRUE, message=TRUE}
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
actdata <- read.csv('activity.csv')
```

##### 2. Transform the interval data.

```{r Transform the interval data}
#actdata$interval <- strptime(gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", actdata$interval), format='%H:%M')
```

-----

## Mean of total number of steps taken per day.

```{r Calculate the ean of total number of steps taken per day}
steps_per_day <- aggregate(actdata$steps, list(actdata$date), FUN=sum, na.rm = TRUE)
colnames(steps_per_day) <- c("Date", "Steps")
```

##### 1. Histogram plotted for the total number of steps taken each day

```{r Plot the histogram for the total number of steps taken each day}
gplot1a <- ggplot(steps_per_day, aes(Steps))
gplot1a+geom_histogram(boundary=0, binwidth=1500, col="Darkblue", fill="lightblue")
+ggtitle("Histogram for the total number of steps taken each day")+xlab("Total steps per day")
+ylab("Frequency using bin width : 1500")+theme(plot.title = element_text(face="bold", size=14))
```

![plot of chunk unnamed-chunk-5](Figure/unnamedchunk-5-1.png)

##### 2. Mean and median of total number of steps taken per day?

```{r Calculate the mean and median of total number of steps taken per day}
steps_mean_per_day <- mean(steps_per_day$Steps , na.rm = TRUE)
steps_median_per_day <- median(steps_per_day$Steps, na.rm = TRUE)
```
* Mean: 9354.2295082
* Median: 10395

-----

## Average of daily activity pattern?

```{r Calculate the average of daily activity pattern}
avgesteps_per_timeblock <- aggregate(x=list(meanSteps=actdata$steps), 
by=list(interval=actdata$interval), FUN=mean, na.rm=TRUE)
```

##### 1. Plot for Time Series

```{r Plot the time Series of daily activity pattern}
timeseriesplot1 <- ggplot(avgesteps_per_timeblock, aes(interval, meanSteps))
timeseriesplot1+geom_line(col="Magenta")+ggtitle("Average steps per time interval")
+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
```

![plot of chunk unnamed-chunk-8](Figure/unnamedchunk-8-1.png)

##### 2. Five Minute Interval on average across all the days in the dataset that contains
##### the maximum number of steps?

```{r Calculate the Five Minute Interval on average across all the days in the dataset}
most_steps <- which.max(avgesteps_per_timeblock$meanSteps)
time_for_most_steps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgesteps_per_timeblock[most_steps,'interval'])
```

* Most Steps at: 8:35

----

## Missing values
##### 1. The total number of missing values in the dataset 

```{r Calculate the total number of missing values in the dataset}
number_of_missing_values <- length(which(is.na(actdata$steps)))
```

* Number of missing values: 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r Create a new dataset that is equal to the original dataset but with the missing data filled in}
activityData_imputed <- actdata
activityData_imputed$steps <- impute(actdata$steps, fun=mean)
``

##### 4. Histogram for the total number of steps taken each day 


```{r Histogram for the total number of steps taken each day }
steps_each_day_imputed <- aggregate(activityData_imputed$steps, list(activityData_imputed$date), FUN=sum)
colnames(steps_each_day_imputed) <- c("Date", "Steps")
totinpstep <- ggplot(steps_each_day_imputed, aes(Steps))
totinpstep+geom_histogram(boundary=0, binwidth=1500, col="darkblue", fill="lightblue")
+ggtitle("Histogram for the total number of steps taken each day (Imputed)")
+xlab("Total steps per day (Imputed)")+ylab("Frequency using bin-width : 1500")
+theme(plot.title = element_text(face="bold", size=14))
```

![plot of chunk unnamed-chunk-12](Figure/unnamedchunk-12-1.png)

##### ... Calculate and report the mean and median total number of steps taken per day. 

```{r Calculate and report the mean and median total number of steps taken per day. }
steps_per_day_mean_imputed <- mean(steps_each_day_imputed$Steps)
steps_per_day_median_imputed <- median(steps_each_day_imputed$Steps)
```
* Mean (Imputed): 1.0766189 &times; 10<sup>4</sup>
* Median (Imputed):  1.0766189 &times; 10<sup>4</sup>

----

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels "weekday" and "weekend"
##### indicating whether a given date is a weekday or weekend.


```{r}
activityData_imputed$DayType <- ifelse(as.POSIXlt(activityData_imputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

##### 2. Panel plot containing a time series plot

```{r Differences in activity patterns between weekdays and weekends}
avgd_activity_DataImputed <- aggregate(steps ~ interval + DayType, data=activityData_imputed, mean)
ggplot(avgd_activity_DataImputed,aes(interval, steps, color= DayType)) + geom_line() + facet_grid(DayType ~ .)
+ggtitle("Plot for difference in activity patterns between weekdays and weekends")+xlab("5 Minute Interval")
+ ylab("Avarage number of steps")+geom_path(inherit.aes = TRUE)
```

![plot of chunk unnamed-chunk-15](Figure/unnamedchunk-15-1.png)
