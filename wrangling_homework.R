#PSYC 259 Homework 2 - Data Transformation
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")
  
### Question 1 ---------- 

#Use glimpse to check the type of "Year". 
#Then, convert it to a numeric, saving it back to 'ds'
#Use typeof to check that your conversion succeeded

#ANSWER
glimpse(ds)
ds$Year <-  as.integer(ds$Year)
typeof(ds$Year)
glimpse(ds)

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER
ds <- rename(ds, rank = Rank, song = Song, artist = Artist, year = Year)
ds
### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

#ANSWER
 ds <-  ds %>% mutate( decade = floor(year, digits = -1))
# I cant get this to work using floor floor requires 1 input and does not allow
# me to change the way if rounds. 
#ds <- ds %>%  mutate(decade = floor(year))
### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER
ds <- arrange(ds, rank)

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER

top10 <-  ds %>% filter( rank <= 10) %>%
               select(artist, song)
### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

#ANSWER
ds_sum <- ds %>% summarize(earliest = min(year, na.rm  = T),
                           most_recent = max(year, na.rm = T),
                           average = mean(year, na.rm = T, n = 4))
ds_sum$average <- round(ds_sum$average)
ds_sum$average <- as.integer(ds_sum$average)
glimpse(ds_sum)
glimpse(ds)

### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year

#ANSWER
  #filter(ds,year== ds_sum)# this should work but doesn't
# try one at a time first 
  #filter(ds,year== ds_sum$earliest)
  #filter(ds,year == ds_sum$most_recent)
  #filter(ds,year == ds_sum$average)
# this works one at a time there are 1, 3, and 11 values I should have total 15
#filter(ds, year == (ds_sum$earliest| ds_sum$most_recent | ds_sum$average))
#filter(ds, year == ds_sum$earliest| ds_sum$most_recent | ds_sum$average)
#doesn't filter anything has 500 values'
#filter(ds, year == ds_sum$earliest, year== ds_sum$most_recent , 
#       year== ds_sum$average)
# does not pick up any values, empty tibble
  #filter(ds, year %in% ds_sum)

# this finally works and has 15 values that I assume are equivalent to the step wise
# now I need to add the arrangement 
ds %>% filter(year %in% ds_sum) %>% arrange(year)

### Question 8 ---------- 

# There's and error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER
ds <- ds %>% mutate(year = ifelse(ds$year==ds_sum$earliest,1979,year))

#ds <- ds %>% mutate(ifelse(year== 1879,1979,year))

# make sure it worked
filter(ds, song=="Brass in Pocket")
# now all the steps together
ds_sum <- ds %>% summarize(earliest = min(year, na.rm  = T),
                           most_recent = max(year, na.rm = T),
                           average = mean(year, na.rm = T, n = 4))
ds_sum$average <- round(ds_sum$average)
ds_sum$average <- as.integer(ds_sum$average)

new_list <-  ds %>% filter(year %in% ds_sum) %>% arrange(year)

### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER
summary <- ds %>% filter( decade != "na") %>% group_by(decade) %>% 
  summarize(mean_rank_decade = mean(rank), N = n())

### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

#ANSWER

 # max_decade <- ds %>%  group_by(decade) %>%  count(decade) %>% 
 #     slice_max(n,n = 1)
# This should be a single row tibble
#max_decade <- ds %>% filter( decade != "na") %>%  group_by(decade) %>%  count(decade) %>% 
 # slice_max(max_decade,n, n = 1 )
# this didn't work either, I will try wrapping the count and slice together

#max_decade <- ds %>% filter( decade != "na") %>%  group_by(decade) %>% 
#  count(decade) 

#max_slice <- slice_max(max_decade$n)
#glimpse(max_decade)

# I think the issue is that I have to ungroup first. Because it is trying to 
# find the max of each group. So I am getting 10 values instead of 1. 

max_decade <- ds %>% filter( decade != "na") %>%  group_by(decade) %>% 
  count(decade) %>% ungroup() %>%  slice_max(max_decade$n)
