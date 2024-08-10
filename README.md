---
output:
  pdf_document: default
  html_document: default
---
## README

---
title: "README.md"
author: "Samson"
date: "2024-08-08"
output: html_document
---

## Installation of the R package library for the Analysis

```{r setup, include=FALSE}
install.packages("tidyverse")
install.packages('sf')
install.packages("ggmap")
install.packages("maps")
install.packages("readr")

knitr::opts_chunk$set(echo = TRUE)
```

## Loading the needed library for the analysis

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(mapview)
library(sf)
library(ggmap)
library(maps)
library(readr)
```
### Combining the 12 csv file into a sigle dataframe for analysis
```{r} 
setwd("C:/Users/Ejiro/Downloads/202305-202404-divvy-tripdata")

csv_merge <- list.files(
  pattern = "*csv",
  full.names = T) %>%
  lapply(read_csv, col_types= cols(.default = "c")) %>%
  bind_rows() %>%
  distinct()
str(csv_merge)

```
### write the merge dataframe into the working directory.
```{r} 
write.csv(csv_merge, "Merge data.csv")
 
```
### Checking for duplicate value in the data set.

```{r}
duplicated(csv_merge)

```
### Removing duplicated data from the data set
```{r}
csv_merge <- unique(csv_merge)

```
### Checking the first six tibble

```{r}
head(csv_merge)

```
### Splitting the started at columns into dates and time.
```{r}
csv_merge$date <- as.Date(csv_merge$started_at)

```
### Generating the month in long format like January, February, etc.

```{r}
csv_merge$month <- format(as.Date(csv_merge$date),"%B") 

```
### Generating day in number

```{r}
csv_merge$day <- format(as.Date(csv_merge$date), "%d")  

```
### Generating year as a four(4) digit year

```{r}
csv_merge$year <- format(as.Date(csv_merge$date), "%Y") 

```

### Extracting the weekday from date using the weekday function

```{r}
csv_merge$day_of_week <- weekdays(csv_merge$date) 

```
### Extracting the Hour, Minutes and Seconds from the Date for the start at time

```{r}
csv_merge$started_at_time <- format(as.POSIXct(csv_merge$started_at),
                                    format = "%Y-%m-%d %H:%M:%S")

```
### Extracting the Hour, Minutes and Seconds from the Date for the ended at time

```{r}
csv_merge$ended_at_time <- format(as.POSIXct(csv_merge$ended_at),
                                  format = "%Y-%m-%d %H:%M:%S")

```
### Calculate the time difference

```{r}
csv_merge$ride_duration <- difftime(csv_merge$ended_at_time, csv_merge$started_at_time, units = "mins")

```
### Checking Minimum and Maximum distance covered 

```{r}
min(csv_merge$ride_duration)

max(csv_merge$ride_duration)

```
### Filter the trip duration with value greater than greater than 2min and less or equal to 24hr

```{r}
csv_merge <-csv_merge %>%
  filter(ride_duration >=2, ride_duration <= (24*60))
  
```

### Count NA values in each column

```{r}
sapply(csv_merge, function(x) sum(is.na(x)))

```

### Removing all null value from the data frame

```{r}
csv_merge <- drop_na(csv_merge)

```

### Removing unneccessary column from the data frame

```{r}
cyclick_df <- subset(csv_merge, select = -c(started_at, ended_at, started_at_time, ended_at_time, start_station_id, end_station_id))

```

### Counting the number of unique value in member casual column

```{r}
cyclick_df %>% count(member_casual)

```

### Counting the number of unique value in rideable type column

```{r}
cyclick_df %>% count(rideable_type)

```
### Counting the number of unique value in start station name and end at station name column

```{r}
cyclick_df %>% count(start_station_name, sort = TRUE)

```

### Checking the minimum and maximum distance covered

```{r}
min(cyclick_df$ride_duration)

max(cyclick_df$ride_duration)

```
### Saving the clean data into file for further analysis.

```{r}
write.csv(cyclick_df, file = "C:/Users/Ejiro/Documents/cyclick_df.csv", row.names = FALSE)

cyclick_df1 <- cyclick_df

```

### Rounding up the ride duration into two decimal places

```{r}
cyclick_df1$ride_duration  <- round(cyclick_df1$ride_duration, digits = 2)

```

###Removing the lat and lon column

#start_lat, start_lng, end_lat, end_lng
cyclick_df2 <- cyclick_df %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
# Analysis
#Fiding the summery statistics of the data
summary(cyclick_df2)

#sumary statistics of the ride duration
summary(cyclick_df2$ride_duration)

#Converting the ride duration to numeric for easy analysis

cyclick_df2$ride_duration <- as.numeric(as.character(cyclick_df2$ride_duration))

#sumary statistics of the ride duration
summary(cyclick_df2$ride_duration)

#Ordering the dataframe based on the days of the week
cyclick_df2$day_of_week <- ordered(cyclick_df2$day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

#Comparing the number of ride per week and asigning it to weekly ride
weekly_ride=cyclick_df2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(day_of_week)

#Checking the first six role for the weekly ride
head(weekly_ride)

#checking the months columns
cyclick_df2 %>% count(month)

# Identifying the top pickup station
                              
#Identifying the top destination station.


#Ordering the dataframe based on the months
cyclick_df2$month <- ordered(cyclick_df2$month, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

#Comparing the number of ride per week and asigning it to weekly ride
monthly_ride=cyclick_df2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(month)

#Checking the first six row for the weekly ride
head(monthly_ride)

#Ordering the dataframe based on the rideable type
cyclick_df2$rideable_type <- ordered(cyclick_df2$rideable_type, levels = c('classic_bike', 'docked_bike', 'electric_bike'))

# Checking the first six row of the dataframe
head(cyclick_df2)

#Comparing the number of ride per week and assigning it to ride type
ride_type=cyclick_df2 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(rideable_type)

#Ride able type
ride_type %>% 
  ggplot(aes(x = rideable_type, y = number_of_ride, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values=c("#33658A", "#2F4858"))

# Monthly ride 
monthly_ride %>%
  ggplot(aes(x = month, y = number_of_ride, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values=c("#33658A", "#2F4858"))+ theme(axis.text.x = element_text(angle = 90))

# Weekly ride
weekly_ride %>% 
  ggplot(aes(x = day_of_week, y = number_of_ride, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=c( "#33658A", "#2F4858"))



cyclick_df %>% filter(member_casual=="member") %>% 
  group_by(start_station_name) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  slice_max(n,n=5) %>% 
  ggplot(mapping = aes(x=start_station_name, y = n))+geom_col(fill = "#686868",width = 0.5)+ labs(title = "Cyclist members most used Start Station",x="Start station",y="Count") + theme(axis.text.x = element_text(angle = 90))


cyclick_df %>% filter(member_casual=="casual") %>% 
  group_by(start_station_name) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  slice_max(n,n=5) %>% 
  ggplot(mapping = aes(x=start_station_name, y = n))+geom_col(fill = "#686868",width = 0.5)+ labs(title = "Cyclist casual members most used Start Station",x="Start station",y="Count") + theme(axis.text.x = element_text(angle = 90))



cyclick_df %>% filter(member_casual=="member" | member_casual=="casual") %>% 
  group_by(end_station_name) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  slice_max(n,n=5) %>% 
  ggplot(mapping = aes(x=end_station_name, y = n))+geom_col(fill = "#686868",width = 0.5)+ labs(title = "Cyclist members most used end Station",x="Start station",y="Count") + theme(axis.text.x = element_text(angle = 90))


cyclick_df %>% filter(member_casual=="casual") %>% 
  group_by(end_station_name) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  slice_max(n,n=5) %>% 
  ggplot(mapping = aes(x=end_station_name, y = n))+geom_col(fill = "#686868",width = 0.5)+ labs(title = "Cyclist casual members most used end Station",x="Start station",y="Count") + theme(axis.text.x = element_text(angle = 90))

#Map
cyclick_df3 <- select(cyclick_df1, start_station_name, start_lat, start_lng)


# If your data contains latitude and longitude
ggplot(cyclick_df3) +
  borders("world", colour = "gray85", fill = "gray80") +
  geom_point(aes(x = start_lng, y = start_lat), color = "blue", size = 2) +
  theme_minimal() +
  labs(title = "Map Plot", x = "Longitude", y = "Latitude")

#New ##

# Define the bounding box for Chicago
chicago_bbox <- c(left = -87.94011, bottom = 41.64454, right = -87.52414, top = 42.02304)

### Plot the map with the data points

```{r}
ggplot(cyclick_df3) +
  borders("state", regions = "illinois", fill = "gray80", colour = "gray85") +
  coord_sf(xlim = c(chicago_bbox["left"], chicago_bbox["right"]), 
           ylim = c(chicago_bbox["bottom"], chicago_bbox["top"]), expand = FALSE) +
  geom_point(aes(x = start_lng, y = start_lat), color = "blue", size = 2) +
  theme_minimal() +
  labs(title = "Chicago Map Plot", x = "Longitude", y = "Latitude")

```


## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
