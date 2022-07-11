library(tidyverse)
library(lubridate)

#load solar data
source("load_data.R")

solar <- function(){
  generation %>%
    rename(generation_source = source_key) %>%
    full_join(weather, by = c("date_time", "plant_id")) %>%
    rename(weather_source = source_key)
}


head(generation)
dim(generation) # 143616      7
head(weather)
dim(weather) # 6528    6

# Raw data counts
range(generation$date_time) # "2020-05-15 00:00:00 UTC" "2020-06-17 23:45:00 UTC"
range(weather$date_time) # "2020-05-15 00:00:00 UTC" "2020-06-17 23:45:00 UTC"
n_distinct(generation$date_time) # 3264
n_distinct(weather$date_time) # 3264
unique(generation$plant_id) # 4135001 4136001
unique(weather$plant_id) # 4135001 4136001
n_distinct(generation$source_key) # 44
n_distinct(weather$source_key) # 2

# NA counts from all sources
gen_na_count <- generation %>%
  group_by(date_time) %>%
  summarize(gen_na_count = sum(is.na(dc_power)))

weather_na_count <- weather %>%
  group_by(date_time) %>%
  summarize(weather_na_count = sum(is.na(ambient_temperature)))

gen_na_count %>%
  full_join(weather_na_count, by = "date_time") %>%
  ggplot(aes(x = date_time)) +
  geom_line(aes(y = gen_na_count, color = "Generation")) +
  geom_line(aes(y = weather_na_count, color = "Weather")) +
  labs(title = "NA and Missing Value Count",
       x = "Date Time", 
       y = "N/A Count")

# Check which rows contain na values
generation %>%
  filter(is.na(dc_power) | 
           is.na(ac_power) | 
           is.na(daily_yield) | 
           is.na(total_yield)) %>%
  nrow() # 7140

weather %>%
  filter(is.na(ambient_temperature) | 
           is.na(module_temperature) | 
           is.na(irradiation)) %>%
  nrow() # 87


# DC Power
generation %>% 
  drop_na() %>% 
  pull(dc_power) %>% 
  summary()

generation %>%
  drop_na() %>%
  ggplot(aes(x = dc_power))+
  geom_histogram()

generation %>%
  drop_na() %>%
  filter(dc_power > 0.0) %>%
  ggplot(aes(x = dc_power))+
  geom_histogram()

generation %>%
  drop_na() %>%
  summarize(no_dc_rate = mean(dc_power == 0)) # 0.495


# AC Power
generation %>% 
  drop_na() %>% 
  pull(ac_power) %>% 
  summary()

generation %>%
  drop_na() %>%
  ggplot(aes(x = ac_power))+
  geom_histogram()

generation %>%
  drop_na() %>%
  filter(ac_power > 0.0) %>%
  ggplot(aes(x = ac_power))+
  geom_histogram()

generation %>%
  drop_na() %>%
  summarize(no_ac_rate = mean(ac_power == 0)) # 0.495


# Daily Yield
generation %>% 
  drop_na() %>% 
  pull(daily_yield) %>% 
  summary()

generation %>%
  drop_na() %>%
  ggplot(aes(x = daily_yield))+
  geom_histogram()

generation %>%
  drop_na() %>%
  filter(daily_yield > 0.0) %>%
  ggplot(aes(x = daily_yield))+
  geom_histogram()

generation %>%
  drop_na() %>%
  summarize(no_daily_yield_rate = mean(daily_yield == 0)) # 0.222

# Total Yield
generation %>% 
  drop_na() %>% 
  pull(total_yield) %>% 
  summary()

generation %>%
  drop_na() %>%
  ggplot(aes(x = total_yield))+
  geom_histogram()

generation %>%
  drop_na() %>%
  filter(total_yield > 0.3) %>%
  ggplot(aes(x = total_yield))+
  geom_histogram() +
  scale_x_log10()

generation %>%
  drop_na() %>%
  summarize(no_total_yield_rate = mean(total_yield == 0)) # 0.00413


# Ambient Temperature
weather %>%
  drop_na() %>%
  pull(ambient_temperature) %>%
  summary()

weather %>%
  drop_na() %>%
  ggplot(aes(x = ambient_temperature)) +
  geom_histogram(bins = 40)

# Module Temperature
weather %>%
  drop_na() %>%
  pull(module_temperature) %>%
  summary()

weather %>%
  drop_na() %>%
  ggplot(aes(x = module_temperature)) +
  geom_histogram(bins = 40)

# Irradiation
weather %>%
  drop_na() %>%
  pull(irradiation) %>%
  summary()

weather %>%
  drop_na() %>%
  ggplot(aes(x = irradiation)) +
  geom_histogram(bins = 40)

weather %>%
  drop_na() %>%
  filter(irradiation > 0.00001) %>%
  ggplot(aes(x = irradiation)) +
  geom_histogram(bins = 40)

weather %>%
  drop_na() %>%
  summarize(no_irradiation_rate = mean(irradiation == 0)) # 0.438



################################################################################
######################     Generation Data      ################################
################################################################################
# view entire month from all sources
generation %>%
  mutate_if(is.numeric, lst(replace_na(., 0))) %>%
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