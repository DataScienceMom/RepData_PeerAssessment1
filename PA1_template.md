Assignment 1 for Reproducible Research
========================================================

## Loading and preprocessing the data

Load data file activity.csv. 


```r
data <- read.csv("activity.csv", header=TRUE, colClasses=c("integer", "Date", "integer"))
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
summary(data)
```

```
## Error in object[[i]]: object of type 'closure' is not subsettable
```

There are missing values only in steps column. 

## What is mean total number of steps taken per day?

Get a new dataset which ignored the missing values. 


```r
data1 <- data[!is.na(data$steps),]
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
summary(data1)
```

```
## Error in summary(data1): object 'data1' not found
```

### Make a histogram of the total number of steps taken each day

Aggregate the data with regards to date. 


```r
stepsDay <- aggregate(data1$steps, by=list(date=data1$date), FUN=sum)
```

```
## Error in aggregate(data1$steps, by = list(date = data1$date), FUN = sum): object 'data1' not found
```

```r
colnames(stepsDay)[2] <- 'steps'
```

```
## Error in colnames(stepsDay)[2] <- "steps": object 'stepsDay' not found
```

```r
hist(stepsDay$steps, main="Histogram of Steps per Day", col="grey", xlab="steps per day", ylab="frequency")
```

```
## Error in hist(stepsDay$steps, main = "Histogram of Steps per Day", col = "grey", : object 'stepsDay' not found
```

### Calculate and report the mean and median total number of steps taken per day

The mean of the total number of steps taken per day

```r
mean(stepsDay$steps)
```

```
## Error in mean(stepsDay$steps): object 'stepsDay' not found
```

The median of the total number of steps taken per day

```r
median(stepsDay$steps)
```

```
## Error in median(stepsDay$steps): object 'stepsDay' not found
```

## What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Aggregate the data with regards to interval. 


```r
stepsInterval <- aggregate(data1$steps, by=list(interval=data1$interval), FUN=mean)
```

```
## Error in aggregate(data1$steps, by = list(interval = data1$interval), : object 'data1' not found
```

```r
colnames(stepsInterval)[2] <- 'average steps'
```

```
## Error in colnames(stepsInterval)[2] <- "average steps": object 'stepsInterval' not found
```

```r
plot(stepsInterval, type="l", main="average steps across all day")
```

```
## Error in plot(stepsInterval, type = "l", main = "average steps across all day"): object 'stepsInterval' not found
```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsInterval[which.max(stepsInterval[,2]),1]
```

```
## Error in eval(expr, envir, enclos): object 'stepsInterval' not found
```


## Imputing missing values

Now we impute the missing values in the original data instead of ignor them.

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)



```r
sum(is.na(data))
```

```
## Warning in is.na(data): is.na() applied to non-(list or vector) of type
## 'closure'
```

```
## [1] 0
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

There are missing values only in steps column. 


```r
summary(data)
```

```
## Error in object[[i]]: object of type 'closure' is not subsettable
```

Use the mean for that 5-minute intervalto impute missing value, which is already calculated in the above question as data frame stepsInterval. 


```r
dataImputed <- data
NAindex <- which(is.na(dataImputed))
```

```
## Warning in is.na(dataImputed): is.na() applied to non-(list or vector) of
## type 'closure'
```

```r
for (i in NAindex){
        dataImputed[i,1]<- stepsInterval[stepsInterval$interval==dataImputed$interval[i],2]
}
summary(dataImputed)
```

```
## Error in object[[i]]: object of type 'closure' is not subsettable
```


### Make a histogram of the total number of steps taken each day

Aggregate the Imputed data with regards to date. 


```r
stepsDayImputed <- aggregate(dataImputed$steps, by=list(date=dataImputed$date), FUN=sum)
```

```
## Error in dataImputed$steps: object of type 'closure' is not subsettable
```

```r
colnames(stepsDayImputed)[2] <- 'steps'
```

```
## Error in colnames(stepsDayImputed)[2] <- "steps": object 'stepsDayImputed' not found
```

```r
hist(stepsDayImputed$steps, main="Histogram of Steps per Day of Imputed Data", col="grey", xlab="steps per day", ylab="frequency")
```

```
## Error in hist(stepsDayImputed$steps, main = "Histogram of Steps per Day of Imputed Data", : object 'stepsDayImputed' not found
```

### Calculate and report the mean and median total number of steps taken per day 

The mean of the total number of steps taken per day of the Imputed data

```r
mean(stepsDayImputed$steps)
```

```
## Error in mean(stepsDayImputed$steps): object 'stepsDayImputed' not found
```

The median of the total number of steps taken per day of the Imputed data

```r
median(stepsDayImputed$steps)
```

```
## Error in median(stepsDayImputed$steps): object 'stepsDayImputed' not found
```

### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

For this particular case, the mean is the same, but the median is different.  Imputing missing data will change the distribution of the total daily number of steps a little bit but not too much.


## Are there differences in activity patterns between weekdays and weekends?

Add variable CalendarDay as factor to data frame dataImputed to state weekdays and weekends. 


```r
w1 <- weekdays(dataImputed$date)
```

```
## Error in dataImputed$date: object of type 'closure' is not subsettable
```

```r
CalendarDay<- rep("weekdays", length(w1))
```

```
## Error in eval(expr, envir, enclos): object 'w1' not found
```

```r
for (i in 1:length(w1)){
        if (w1[i]=="Saturday" | w1[i]=="Sunday"){
                CalendarDay[i] <- "weekends"
        }
}
```

```
## Error in eval(expr, envir, enclos): object 'w1' not found
```

```r
dataImputed$CalendarDay <- as.factor(CalendarDay)
```

```
## Error in is.factor(x): object 'CalendarDay' not found
```

Get average daily activity data for the imputed data and plot the average daily activity pattern with regards to weekdays and weekends.


```r
stepsIntervalImputed <- aggregate(dataImputed$steps, by=list(interval=dataImputed$interval, CalendarDay= dataImputed$CalendarDay), FUN=mean)
```

```
## Error in dataImputed$steps: object of type 'closure' is not subsettable
```

```r
colnames(stepsIntervalImputed)[3] <- 'AverageSteps'
```

```
## Error in colnames(stepsIntervalImputed)[3] <- "AverageSteps": object 'stepsIntervalImputed' not found
```

```r
library(lattice)
xyplot(AverageSteps ~ interval|CalendarDay, data=stepsIntervalImputed, layout=c(1,2), type="l", xlab="Intervals", ylab="average steps", main="Comparison of Average Daily Activity Pattern between Weekdays and Weekends")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'stepsIntervalImputed' not found
```

