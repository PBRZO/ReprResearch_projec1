---
title: "reproducible_research_week2"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
```
Read the csv file 
```{r}
activity <-read.csv("activity.csv")
```

Group by date (as.DATE) and sum up steps per day
```{r}
summary <- activity %>% group_by(as.Date(date))%>% tally(steps)
```

Generate histogram
```{r}
hist(summary$n,main="Histogram of daily steps",xlab="steps")
```
Average and median daily steps are

```{r}
mean(summary$n)
median(summary$n)
```
,respectively.

The following graph show the average daily steps by inerval:

```{r}
interval_mean <- activity %>%na.omit() %>% group_by(interval)%>%summarize_at(vars(-date),list(mean=mean))

plot(interval_mean$interval,interval_mean$mean,xlab = 'interval',ylab='average',main="daily average steps ")
```
There are 
```{r}
nrow(activity[is.na(activity),])
```
missing values.
```{r}
max(interval_mean$mean)
```
Imputing na in steps with mean of steps in same interval:

```{r}
activity_interval_mean <- merge(activity,interval_mean,by="interval")
for(i in 1:nrow(activity_interval_mean )){if(is.na(activity_interval_mean[i,2])){activity_interval_mean[i,2] <- activity_interval_mean[i,4]}}
summary_aim <- activity_interval_mean %>% group_by(as.Date(date))%>% tally(steps)

mean(summary$n)
median(summary$n)
mean(summary_aim$n)
median(summary_aim$n)
```
```{r}
activity_interval_mean_mod <- activity_interval_mean %>% mutate(weekday =weekdays(as.Date(date))) %>% filter(weekday %in% (c("Monday","Tuesday","Wednesday","Thursday","Friday"))) %>% mutate(weekday="workday")
activity_interval_mean_mod <- rbind(activity_interval_mean_mod,  activity_interval_mean %>% mutate(weekday =weekdays(as.Date(date))) %>% filter(weekday %in% (c("Saturday","Sunday"))) %>% mutate(weekday="weekend"))
```

Generating the plot
```{r}
ggplot(activity_interval_mean_mod,aes(x=interval,y=mean)) + theme(axis.text.x=element_text(angle=90)) + geom_point()+facet_wrap(~weekday)
```
It doesn't appear that there is a difference between weekend and weekdays
