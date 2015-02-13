---
title: "Assignment_1"
author: "mnf2014"
date: "02/13/2015"
output: html_document
---
#Reproducible Research
##Assignment 1 
This exercise is based on the data from a personal activity monitoring device. The variables are: steps, date and interval in which the steps were measured.

##Loading and preprocessing the data  


```r
datain<-read.csv("activity.csv",header=TRUE,stringsAsFactors=FALSE)
datain$date<-as.Date(datain$date)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
##What is mean total number of steps taken per day?  
In order to know the total number of steps taken per day I will use the function aggregate with the function sum.


```r
options(scipen = 1, digits = 2)
step_per_day<-aggregate(datain$steps,list(datain$date),sum)
head(step_per_day)
```

```
##      Group.1     x
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
hist(step_per_day$x,main="Histogram of total number of steps per day",breaks=seq(0,25000,l=26),xlab="Total steps per day",col="green")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
stpmean<-mean(step_per_day$x,na.rm=TRUE)
stpmedian<-median(step_per_day$x,na.rm=TRUE)
```
 The value obtained for the mean corresponds to 10766.19 and for the median is 10765. Both of them are in accordance with the histogram where the median of total number of steps taken per day is around 11000.  

 
## What is the average daily activity pattern?  

```r
library(dplyr)
byint<-group_by(datain,interval)
byintmean<-summarise(byint,mean(steps,na.rm=TRUE))
plot(byintmean$interval,byintmean$mean,type="l",col="red",xlab="interval",ylab="Average number of steps",xlim=c(0,2500),ylim=c(0,200))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
meanmax<-max(byintmean$mean)
indmax<-which(byintmean$mean>=206.1695,arr.ind=TRUE)
byintmean[101:105,]
```

```
## Source: local data frame [5 x 2]
## 
##   interval mean(steps, na.rm = TRUE)
## 1      820                       171
## 2      825                       155
## 3      830                       177
## 4      835                       206
## 5      840                       196
```
From the previous analysis the interval that contains the maximum number of steps is the 835.

 
##Imputing missing values  

In order to calculate the number of NA values I have performed the following lines of code.

```r
testna<-is.na(datain$steps)
totalNA<-sum(testna)
```

The total number of NA values in the steps register is 2304, which corresponds to  eight days without measurements, the 13% of the total rows (17568).

Now, in the following point, I understand that we have to replace the NA with a value that I obtained approximatively taking into account the others values. So, I will essay two ways. 1) At the begining of the exercice I have obtained the total number of steps per day, from there I can see that only eight days have NA values, then I decided to replace these NA values with the previous/next value different to NA. 2) This second way is the right one to continue with the others assigments points and consist on replacing the NA steps values of the eight days by the  values previously obtained by the mean function.


```r
options(scipen = 1, digits = 2)
#1
days<-c(126,11015,15414,10571,3219,3219,7336,7047)
step_per_day[is.na(step_per_day)]<-days
hist(step_per_day$x,main="1)Histogram of total number of steps per day without NA",breaks=seq(0,25000,l=26),xlab="Total steps per day",col="blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
stpmean1<-mean(step_per_day$x,na.rm=TRUE)
stpmedian1<-median(step_per_day$x,na.rm=TRUE)

#2
datanew<-datain %>% 
    group_by(interval) %>% 
    mutate(steps = ifelse(is.na(steps),as.integer(mean(steps, na.rm=TRUE)),steps))
step_per_day2<-aggregate(datanew$steps,list(datanew$date),sum)
hist(step_per_day2$x,main="2)Histogram of total number of steps per day without NA",breaks=seq(0,25000,l=26),xlab="Total steps per day",col="grey")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png) 

```r
stpmean2<-mean(step_per_day2$x,na.rm=TRUE)
stpmedian2<-median(step_per_day2$x,na.rm=TRUE)
```
For the first approximation the value of mean corresponds to 10304.18 and the value of median to 10571. In the second approach I have obtained a mean value of 10749.77 and a median of 10641. In the first case the values have slightly decreased with respect to the data containing NA values. However, in the second approach, the results are closer to those with NA values.  

##Are there differences in activity patterns between weekdays and weekends?


```r
library(ggplot2)
wend<-c("weekend")
wday<-c("weekday")
datawd<-datanew %>% mutate(wdays = ifelse(weekdays(date,abbreviate=TRUE)=="Sun"|weekdays(date,abbreviate=TRUE)=="Sat",wend,wday))
dataplot<-summarise(group_by(datawd,interval,wdays),m=mean(steps))
qplot(interval,m,data=dataplot,facets=.~wdays,geom="line",color=wdays,ylab="average of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
   Yes, from figures is easy to observe that in weekend days the activity start later than in week days. Also in weekend days the average number of steps is higher than week days.

