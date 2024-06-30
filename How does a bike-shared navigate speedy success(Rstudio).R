install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# Combining the 12 csv file into a sigle dataframe for analysis #

setwd("C:/Users/Ejiro/Downloads/202305-202404-divvy-tripdata")

csv_merge <- list.files(
  pattern = "*csv",
  full.names = T) %>%
  lapply(read_csv, col_types= cols(.default = "c")) %>%
  bind_rows() %>%
  distinct()
str(csv_merge)

# write the merge dataframe into the working directory.

write.csv(csv_merge, "Merge data.csv")

# Checking for duplicate value in the data set.

duplicated(csv_merge)
 
#Removing duplicated data from the data set.

csv_merge <- unique(csv_merge)

#Checking the first six tibble

head(csv_merge)

# Splitting the started at columns into dates and time.

csv_merge$date <- as.Date(csv_merge$started_at)

#Generating the month in long format like January, February, etc.
csv_merge$month <- format(as.Date(csv_merge$date),"%B") 

#Generating day in number
csv_merge$day <- format(as.Date(csv_merge$date), "%d")  

#Generating year as a four(4) digit year
csv_merge$year <- format(as.Date(csv_merge$date), "%Y") 

#Extracting the weekday from date using the weekday function
csv_merge$day_of_week <- weekdays(csv_merge$date) 

# Extracting the Hour, Minutes and Seconds from the Date for the start at time
csv_merge$started_at_time <- format(as.POSIXct(csv_merge$started_at),
                                    format = "%Y-%m-%d %H:%M:%S")

# Extracting the Hour, Minutes and Seconds from the Date for the ended at time
csv_merge$ended_at_time <- format(as.POSIXct(csv_merge$ended_at),
                                  format = "%Y-%m-%d %H:%M:%S")
# Calculate the time difference
csv_merge$ride_duration <- difftime(csv_merge$ended_at_time, csv_merge$started_at_time, units = "mins")

#Checking Minimum and Maximum distance covered 
min(csv_merge$ride_duration)

max(csv_merge$ride_duration)

#Filter the trip duration with value greater than greater than 2min and less or equal to 24hr
csv_merge <-csv_merge %>%
  filter(ride_duration >=2, ride_duration <= (24*60))


#count NA values in each column
sapply(csv_merge, function(x) sum(is.na(x)))

#Removing all null value from the data frame
csv_merge <- drop_na(csv_merge)

#Removing unneccessary column from the data frame
cyclick_df <- subset(csv_merge, select = -c(started_at, ended_at, started_at_time, ended_at_time, start_station_id, end_station_id))

# Counting the number of unique value in member casual column
cyclick_df %>% count(member_casual)

# Counting the number of unique value in rideable type column
cyclick_df %>% count(rideable_type)

# Counting the number of unique value in start station name and end at station name column
cyclick_df %>% count(start_station_name, sort = TRUE)

#Checking the minimum and maximum distance covered

min(cyclick_df$ride_duration)

max(cyclick_df$ride_duration)

# Saving the clean data into file for further analysis.
write.csv(cyclick_df, file = "C:/Users/Ejiro/Documents/cyclick_df.csv", row.names = FALSE)

cyclick_df1 <- cyclick_df

#Rounding up the ride duration into two decimal places
cyclick_df1$ride_duration  <- round(cyclick_df1$ride_duration, digits = 2)

