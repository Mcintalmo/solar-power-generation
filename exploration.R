library(tidyverse)
library(lubridate)

#load solar data
source("load_data.R")

solar <- function(){
  start_date <- min(generation$date_time)
  end_date <- max(generation$date_time)
  date_times <- seq(from = start_date, to = end_date, by='15 mins') # lubridate
  
  tibble(date_time = date_times) %>%
    full_join(generation, by = "date_time") %>% 
    rename(generation_source = source_key) %>%
    full_join(weather, by = c("date_time", "plant_id")) %>%
    rename(weather_source = source_key)
}

generation %>%
  filter(plant_id == unique(generation$plant_id)[1]) %>%
  group_by(source_key) %>%
  tally()



head(generation)
dim(generation) # 136476     11
head(weather)
dim(weather) # 6441    6

# Raw data counts
range(generation$date_time) # "2020-05-15 00:00:00 UTC" "2020-06-17 23:45:00 UTC"
range(weather$date_time) # "2020-05-15 00:00:00 UTC" "2020-06-17 23:45:00 UTC"
n_distinct(generation$date_time) # 3263
n_distinct(weather$date_time) # 3262
unique(generation$plant_id) # 4135001 4136001
unique(weather$plant_id) # 4135001 4136001
n_distinct(generation$source_key) # 44
n_distinct(weather$source_key) # 2

# Data counts from all sources
gen_count <- generation %>%
  group_by(date_time) %>%
  summarize(gen_count = n())

weather_count <- weather %>%
  group_by(date_time) %>%
  summarize(weather_count = n())

gen_count %>%
  full_join(weather_count, by = "date_time") %>%
  ggplot(aes(x = date_time)) +
  geom_line(aes(y = gen_count, color = "gen_count")) +
  geom_line(aes(y = weather_count, color = "weather_count"))


# Cleaning up NA's

solar() %>%
  is.na() %>%
  sum() # 151

# Check which rows contain na values
solar() %>%
  filter(is.na(generation_source)) %>%
  is.na() %>%
  sum() # 135

solar() %>%
  filter(is.na(c(weather_source))) %>%
  is.na() %>%
  sum() # 26
# all NA's found

# They all happened at the same time. Pull the date
na_date_time <- solar() %>%
  filter(is.na(weather_source)) %>%
  pull(date_time) %>%
  unique()

na_date_time # "2020-05-15 23:15:00 UTC" "2020-06-03 14:00:00 UTC"

na_date_time <- na_date_time[1]

# Which source did it come from?
solar() %>%
  group_by(plant_id, weather_source) %>%
  summarize()
# Most likely HmiyD2TTLFNqkNe, because of the plant_id

# Can we fill in the NA?
# We will use the average of the data recorded before and after the missing observation
missing_observation <- weather %>%
  filter(source_key == "HmiyD2TTLFNqkNe") %>%
  filter(date_time >= na_date_time - minutes(15) &
           date_time <= na_date_time + minutes(15)) %>%
  summarize(date_time = na_date_time,
            plant_id = plant_id[1],
            source_key = source_key[1],
            ambient_temperature = mean(ambient_temperature),
            module_temperature = mean(module_temperature),
            irradiation = mean(irradiation))

weather <- weather %>%
  add_row(missing_observation,
          .after = which(weather$date_time == na_date_time - minutes(15)))

rm(na_date_time, missing_observation)

# Check the combined data to confirm NA's were reduced
solar() %>%
  is.na() %>%
  sum() 

# The remaining NAs are left in to indicate the system was down at the time


# DC Power
range(generation$dc_power) #  0.00 14471.12
mean(generation$dc_power) # 1708.541
sd(generation$dc_power) # 3222.181
median(generation$dc_power) # 6.05


# AC Power
range(generation$ac_power) #  0.00 1410.95
mean(generation$ac_power) # 274.8035
sd(generation$ac_power) # 380.1826
median(generation$ac_power) # 3.506905


# Daily Yield
range(generation$daily_yield) # 0 9873
mean(generation$daily_yield) # 3295.434
sd(generation$daily_yield) # 3035.294
median(generation$daily_yield) # 2834.804


# Total Yield
range(generation$total_yield) # 0 2247916295
mean(generation$total_yield) # 330382090
sd(generation$total_yield) # 608570527
median(generation$total_yield) # 7269333

################################################################################
######################     Generation Data      ################################
################################################################################
# view entire month from all sources
solar() %>%
  mutate_if(is.numeric, funs(replace_na(., 0))) %>%
  group_by(generation_source) %>%
  ggplot(aes(x = date_time, y = dc_power, fill = generation_source)) +
  geom_area(position = "stack") +
  theme(legend.position = "none")

# View entire month average
generation %>%
  group_by(date_time) %>%
  summarize(avg_dc_power = mean(dc_power)) %>%
  ggplot(aes(x = date_time, y = avg_dc_power)) +
  geom_area()

# view all sources for one day
generation %>%
  filter(date_time < make_date(2020, 05, 16)) %>%
  group_by(source_key) %>%
  ggplot(aes(x = date_time, y = dc_power, fill = source_key)) +
  geom_area(position = "stack") +
  theme(legend.position = "none")

# view entire month from one source
generation %>%
  filter(source_key == unique(source_key)[1]) %>%
  ggplot(aes(x = date_time, y = dc_power)) +
  geom_area()

# view one day from one source
generation %>%
  filter(source_key == unique(source_key)[1]) %>%
  filter(date_time < make_date(2020, 05, 16)) %>%
  ggplot(aes(x = date_time, y = dc_power)) +
  geom_area()


################################################################################
######################       Weather Data     ##################################
################################################################################
# view entire month from both sources
generation %>%
  group_by(source_key) %>%
  ggplot(aes(x = date_time, y = dc_power, fill = source_key)) +
  geom_area(position = "stack") +
  theme(legend.position = "none")