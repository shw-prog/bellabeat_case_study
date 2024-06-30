# bellabeat_case_study

---
title: "Bellabeat-capstone"
author: "shweta"
date: "2023-06-19"
output: html_document
---
# Bellabeat Case Study

This case study is a part of Google Data Analytics course on Coursera.  

## Scenario  

Bellabeat is a small successful high-tech manufacturer of health-focused products for women but they have potential of becoming a larger player in the global smart device market. In this case study, I am going to analyze smart device data to gain insight into how customers use their smart devices which will help guide marketing strategy for the company.  

## Ask

### Business Task

#### Guiding questions:
1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?  

The task is to analyse non-Bellabeat smart device usage data in order to gain insight into how customers use their smart devices.  

## Prepare

### Data Sources Used:

Fitbit Fitness Tracker Data(CCO:Public Domain, dataset made available through Mobius): This Kaggle data set contains personal fitness tracker from thirty fitbit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and heart rate that can be used to explore users’ habits.  

#### Data Integrity:
**Reliable**- Yes
**Original**- Yes
**Comprehensive**- No. Data is taken from only 30 users and consists information from a mere 1 month, which is not enough to make accurate conclusions.
**Current**- No. Data is collected from 4/12/2016 to 5/12/2016
**Cited**- Yes

## Process

I will be using R for this case study today as I will be able to clean, analyse and create visualization in the same platform.  

First, I will be installing and loading all the packages I need.  
```{r}
options(repos = list(CRAN="http://cran.rstudio.com/"))
install.packages("tidyverse")
install.packages("dplyr")
install.packages("skimr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("scales")
install.packages("lubridate")
library(tidyverse)
library(dplyr)
library(skimr)
library(janitor)
library(ggplot2)
library(scales)
library(lubridate)

```
Then, I will be importing the data required.  

```{r}
full_data <-read_csv(".kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
Sleep <- read_csv(".kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
Weight <-read_csv(".kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
```

And then, we'll take a peek at the data we imported.  
```{r}
head(full_data)
head(Sleep)
head(Weight)
```

### Sorting, filtering and cleaning data:
I noticed there are some null values, which I will be filtering out and then also organizing the date format here.

```{r}
merged_data <-full_data %>%
  filter(TotalSteps > 0)
merged_data$ActivityDate=as.POSIXct(merged_data$ActivityDate, format="%m/%d/%y")
head(merged_data)

Sleep$SleepDay=as.POSIXct(Sleep$SleepDay, format="%m/%d/%y")
head(Sleep)

```
Let's check the number of unique values.  
```{r}
(n_unique(merged_data$Id))
(n_unique(Sleep$Id))
(n_unique(Weight$Id))
```
We understand that we have data from only 24 users for sleep and 8 users who logged in their weight which will decrease the credibility of the findings. We will not be analysing data with weight due to the very low sample size.

## Analyze and Share

10,000 steps per day is frequently cited as a healthy number of steps to aim for. A study published in March 24, 2020, in the Journal of the American Medical Association found that compared with taking 4,000 steps per day, a number considered to be low for adults, taking 8,000 steps per day was associated with a 51% lower risk for all-cause mortality (or death from all causes). Taking 12,000 steps per day was associated with a 65% lower risk compared with taking 4,000 steps. The authors saw no association between step intensity and risk of death. I wanted to see if people using the fitness tracker are meeting this standard.   
```{r}
steps <- merged_data %>%
  group_by(Id)%>%
  summarize(average_steps=mean(TotalSteps))
head(steps)
ggplot(data = steps) + geom_histogram(mapping = aes(x = average_steps), fill = "orange") + labs(title= "Average Steps Taken in a Day")+ theme(plot.title = element_text(hjust = 0.5))
```
As you can see, most people fall between the 4000-12000 range with a few ouliers below 4000 and few over 12000 as well.
It would be nice to visualize this as a pie chart to see what percentage of people are at a healthier range.  
```{r}
s_labels <- c("0-4000", "4000-8000", "8000-10000", "10000-12000", "12000+")
steps$s_groups <- cut(steps$average_steps, breaks = c(0, 4000, 8000, 10000, 12000, Inf), labels=s_labels, right = TRUE)
steps <- steps[order(steps$s_groups), ] 

steps_by_group <- steps %>% 
  count(s_groups)%>%
  mutate(prop = n/sum(n)*100)
steps_by_group
ggplot(steps_by_group, aes(x="", y=n, fill=s_groups)) +
  geom_bar(stat = "identity", width=1, color="white") +
  coord_polar("y", start=0)+ labs(title = "Number of Steps per Day") + theme(plot.title = element_text(hjust = 0.5))
```
We can now see that although most people walk 4000-8000 steps a day, almost 50% of the users are walking at a healthy range of 8000 and higher.  

Lets see what percentage of the people were meeting CDC guidelines for activity per week. Recommended levels for health benefits is moderate intensity activity for 150 minutes a week or vigorous intensity activity for 75 minutes a week combined with some muscle strengthening activities.  

```{r}
healthy <- sum(Activity$VeryActiveMinutes_week >= 75 | Activity$FairlyActiveMinutes_week >=150)
percentage_of_healthy <- healthy/33 * 100
head(percentage_of_healthy)

activity_chart <- data.frame(
  Groups = c("Met_CDC_standards", "Did_not_meet"),
  HealthStandards = c(healthy, 33 - healthy)
)
head(activity_chart)
ggplot(activity_chart, aes(x="", y=HealthStandards, fill= Groups))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + labs(title = "CDC Recommended Activity Levels") + theme(plot.title = element_text(hjust = 0.5))

```
We can see that 54.5% of users met CDC guidelines which is 18 of the 33 people.  

Now, let's check whether people are burning more calories on any particular day of the week.  
```{r}
dates <- merged_data
dates$weekday <- weekdays(dates$ActivityDate)                 
head(dates)
dates$weekday <- factor(dates$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(dates, aes(x= weekday, y= Calories, color= weekday)) + geom_point(stat= "identity",position = "jitter",alpha = 0.3 ) + labs(title = "Calories Burnt Based on Days of the Week") + theme(plot.title = element_text(hjust = 0.5))


```
Some tend to burn more calories Tuesdays to Thursdays and then again some burned less calories on Tuesday. It seems more consistent on the weekend within a range of 1200 to 4000 calories.  

Let us take a look at the sleep data we have and check how many hours on average people sleep in a day.
```{r}
Sleep1<- Sleep %>%
  mutate(HoursAsleep = TotalMinutesAsleep/60)%>%
  select(Id, SleepDay, HoursAsleep)
ggplot(data = Sleep1, mapping = aes(x=SleepDay, y=HoursAsleep, color= HoursAsleep)) + geom_point() + scale_y_continuous(breaks = c(2, 4, 6, 8,10,12,16))+ labs(title = "Hours of Sleep in a Day") + theme(plot.title = element_text(hjust = 0.5))

```
It looks like most people sleep between 5 and 9 hours.  
I was also curious to see if there is any relationship between sleep and activity and activity intensity.  
```{r}
Activity1 <- Sleep %>%
  group_by(Id)%>%
  summarize(AvgSleepPerDay = mean(TotalMinutesAsleep/60))
head(Activity1)


Activity <- merged_data%>%
  select(Id, ActivityDate, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes)%>%
  group_by(Id)%>%
  summarize(VeryActiveMinutes_week=mean(VeryActiveMinutes) * 7, FairlyActiveMinutes_week =mean(FairlyActiveMinutes) *7, LightlyActiveMinutes_week = mean(LightlyActiveMinutes) *7)
head(Activity)
Activity_sleep <- merge(x=Activity, y=Activity1, by="Id")
head(Activity_sleep)
ggplot(data = Activity_sleep, mapping = aes(x=AvgSleepPerDay, y=VeryActiveMinutes_week, color = AvgSleepPerDay)) + geom_point()+ labs(title = "Sleep vs Very Active Minutes") + theme(plot.title = element_text(hjust = 0.5))
ggplot(data = Activity_sleep, mapping = aes(x=AvgSleepPerDay, y=FairlyActiveMinutes_week, color = AvgSleepPerDay)) + geom_point()+ labs(title = "Sleep vs Fairly Active Minutes") + theme(plot.title = element_text(hjust = 0.5))
ggplot(data = Activity_sleep, mapping = aes(x=AvgSleepPerDay, y=LightlyActiveMinutes_week, color = AvgSleepPerDay)) + geom_point()+ labs(title = "Sleep vs Lightly Active Minutes") + theme(plot.title = element_text(hjust = 0.5))

```
Users spending more time being very active and moderately active seem to spend 6-8hrs sleeping whereas people who spend more time being lightly active spend 6-9 hrs sleeping. The data available does not demonstrate a big enough difference to make a conclusion. However, insight from a bigger data source could be promising.    

Now let's see if people sleep more or less on certain days of the week.    
```{r}
Sleep$weekday <- weekdays(Sleep$SleepDay)
head(Sleep)
Sleep$weekday <- factor(Sleep$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

SleepPerDay <-Sleep%>%
  mutate(HoursAsleep = TotalMinutesAsleep/60)%>%
  select(Id, weekday, HoursAsleep)
head(SleepPerDay)

ggplot(data = SleepPerDay, mapping = aes(x=weekday, y=HoursAsleep, color= HoursAsleep)) + geom_point(stat= "identity",position = "jitter",alpha = 0.3 ) + scale_y_continuous(breaks = c(2, 4, 6, 8,10,12,16))+ labs(title = "Hours of Sleep Based on Days of the Week") + theme(plot.title = element_text(hjust = 0.5))


```
Although, there seem to be a few who tend to sleep more on Thursday and Friday. I was surprised to find that on the weekend again, similar to calories burnt, people sleep a more consistent 5-9 hours.    

### Summary:
1. People tend not to log in their weight.
2. Most users walk 4-8000 steps a day. A low percentage walk less than 4000 and the rest walk more than 8000 steps.
3. More than half of the users meet CDC guidelines for weekly activity.
4. Calories burnt during the week were a pretty consistent 1500-4000 on Saturday and Sunday and a little spread out during the weekdays with some highs seen on Thursday and Friday although the correlation should be taken with a grain of salt.
5. Users tend to sleep between 5-9 hrs every night and our data did not show much of a correlation with intensity of exercise.
6. Data also did not show much of a correlation between days of the week and sleep besides hours of sleep being a more consistent 5-9hrs on the weekend and a slightly downward trend around the weekend compared to the rest of the week.

## Act

### Recommendations:
1. Users tend not to log in their weight on the Fitness Tracker. Perhaps giving people easier options of logging in weight like a way to sync with a weighing machine or reminders to log in weight at certain intervals would act as an advantage in the Bellabeat app and Leaf, especially for customers who have weight loss goals.
2. Since most users have a healthy step count, they might be tracking them on a regular basis and may be setting stepping goals as well. Positive reinforcement with notifications for motivation to meet goals and acknowledgement and praise for meeting such goals in the app might be appealing to customers.
3. The opportunity to set personal fitness goals as well as goals determined by CDC, NIH or other respectable medical research community may be included in the app. General health related information based on age, gender etc specific to the user provided on the app might generate interest as well.
4. A calculator to add food consumed in order to determine caloric intake vs calories burned during the day as tracked through the Leaf will benefit users trying to maintain caloric intake or weight.
5. Tracking sleep would benefit if the Leaf and app could also determine REM cycles. This may be possible if heart rate and respiratory rate can be measured along with the time the user falls asleep and wakes up. The user can track REM cycles and determine if they need to change their habits or environment to achieve better sleep. 
6. This data did not provide information on the gender and age of the users which could make a vast difference in the applicability of the analysis towards the target population of Bellabeat which is primarily women. Health and fitness requirements and habits of women may be different from men. Having any medical condition would also make a difference. Medical data of the sample population was also not provided.  

Overall, there is a lot to learn from this data but results would be more reliable with a larger sample size and a longer duration of records.
