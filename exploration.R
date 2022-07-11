library(tidyverse)
library(lubridate)

# TODO: Define all variables present, based on original data set
# TODO: Check Correlation between weather sensor data and output
# TODO: Create validation set (last three days of the month)
# TODO: Create train and test set

################################################################################
# EXTRACT, TRANSFORM, LOAD
################################################################################
parse_generation_data <- function(file_name,
                                  date_format = "ymd HM",
                                  data_path = "./data/") {
  read.csv(paste0(data_path, file_name)) %>%
    as_tibble() %>%
    # For some reason, the two files use different date formatting
    mutate(date_time = parse_date_time(DATE_TIME, date_format),
           plant_id = as.character(PLANT_ID)) %>%
    select(-DATE_TIME, -PLANT_ID) %>%
    rename(source_key = SOURCE_KEY,
           dc_power = DC_POWER,
           ac_power = AC_POWER,
           daily_yield = DAILY_YIELD,
           total_yield = TOTAL_YIELD) %>%
    relocate(date_time,
             plant_id,
             source_key,
             dc_power,
             ac_power,
             daily_yield,
             total_yield)
}


parse_weather_data <- function(file_name, data_path = "./data/") {
  read.csv(paste0(data_path, file_name)) %>%
    as_tibble() %>%
    mutate(date_time = parse_date_time(DATE_TIME, ("ymd HMS")),
           plant_id = as.character(PLANT_ID)) %>%
    select(-DATE_TIME, -PLANT_ID) %>%
    rename(source_key = SOURCE_KEY,
           ambient_temperature = AMBIENT_TEMPERATURE,
           module_temperature = MODULE_TEMPERATURE,
           irradiation = IRRADIATION) %>%
    relocate(date_time,
             plant_id,
             source_key,
             ambient_temperature,
             module_temperature,
             irradiation)
}


generation <- 
  parse_generation_data("Plant_1_Generation_Data.csv", "dmy HM") %>%
  bind_rows(parse_generation_data("Plant_2_Generation_Data.csv", "ymd HMS"))


weather <- parse_weather_data("Plant_1_Weather_Sensor_Data.csv") %>%
  bind_rows(parse_weather_data("Plant_2_Weather_Sensor_Data.csv"))


# Contains a large number of NA rows,
#  and it is sometimes not useful to include them, or have a lot of 
#  na.remove = TRUE
date_time <- seq(from = min(generation$date_time, weather$date_time), 
                  to = max(generation$date_time, weather$date_time), 
                  by='15 mins') # lubridate

generation_sources <- generation %>%
  select(plant_id, source_key) %>%
  rename(generation_source = source_key) %>%
  unique()

weather_sources <- weather %>%
  select(plant_id, source_key) %>%
  rename(weather_source = source_key) %>%
  unique()

solar <- generation_sources %>%
  full_join(weather_sources, by = "plant_id") %>%
  expand_grid(date_time) %>%
  full_join(generation, by = c("date_time", 
                               "plant_id", 
                               "generation_source" = "source_key")) %>%
  full_join(weather, c("date_time", 
                       "plant_id", 
                       "weather_source" = "source_key")) %>%
  relocate(date_time)


head(generation)
dim(generation) # 136476      7
head(weather)
dim(weather) # 6441    6
head(solar)
dim(solar) # 143616     11
# Discrepency in row counts has to do with missing rows from generation data

object.size(generation)
object.size(weather)
object.size(solar)


rm(generation_sources, weather_sources, date_time, parse_generation_data, 
   parse_weather_data)




################################################################################
############################### EXPLORATION ####################################
################################################################################
# Raw data counts
range(generation$date_time) # "2020-05-15 00:00:00 UTC" "2020-06-17 23:45:00 UTC"
range(weather$date_time) # "2020-05-15 00:00:00 UTC" "2020-06-17 23:45:00 UTC"
n_distinct(generation$date_time) # 3263
n_distinct(weather$date_time) # 3262
unique(generation$plant_id) # 4135001 4136001
unique(weather$plant_id) # 4135001 4136001
n_distinct(generation$source_key) # 44
n_distinct(weather$source_key) # 2


################################################################################
################################ NA VALUES #####################################
################################################################################
gen_na_count <- solar %>%
  group_by(date_time, generation_source) %>%
  summarize(gen_na_count = sum(is.na(dc_power)))

weather_na_count <- solar %>%
  group_by(date_time, weather_source) %>%
  summarize(weather_na_count = sum(is.na(ambient_temperature)))

gen_na_count %>%
  group_by(generation_source) %>%
  ggplot(aes(x = date_time, y = gen_na_count, color = generation_source, fill = generation_source)) +
  geom_area(position = "stack") +
  theme(legend.position = "none")
# There are a number of NAs, certain sources appera to be down for a week,
#  sometimes groups go down
          
weather_na_count %>%
  group_by(weather_source) %>%
  ggplot(aes(x = date_time, y = weather_na_count, color = weather_source, fill = weather_source)) +
  geom_area()
  labs(title = "Weather NA and Missing Value Count",
       x = "Date Time", 
       y = "N/A Count")

# Check which rows contain na values
solar %>%
  filter(is.na(dc_power) | 
           is.na(ac_power) | 
           is.na(daily_yield) | 
           is.na(total_yield)) %>%
  nrow() # 7140

solar %>%
  filter(is.na(ambient_temperature) | 
           is.na(module_temperature) | 
           is.na(irradiation)) %>%
  nrow() # 1914

solar %>%
  filter(is.na(dc_power) &
           is.na(ac_power) &
           is.na(daily_yield) &
           is.na(total_yield) &
           is.na(ambient_temperature) &
           is.na(module_temperature) &
           is.na(irradiation)) %>%
  nrow() # 1910


################################################################################
######################     Generation Data      ################################
################################################################################

# DC Power
generation %>% 
  pull(dc_power) %>% 
  summary()

generation %>%
  ggplot(aes(x = dc_power))+
  geom_histogram()

generation %>%
  filter(dc_power > 0.0) %>%
  ggplot(aes(x = dc_power))+
  geom_histogram()

generation %>%
  summarize(no_dc_rate = mean(dc_power == 0)) # 0.495


# AC Power
generation %>% 
  pull(ac_power) %>% 
  summary()

generation %>%
  ggplot(aes(x = ac_power))+
  geom_histogram()

generation %>%
  filter(ac_power > 0.0) %>%
  ggplot(aes(x = ac_power))+
  geom_histogram()

generation %>%
  summarize(no_ac_rate = mean(ac_power == 0)) # 0.495


# Daily Yield
generation %>% 
  pull(daily_yield) %>% 
  summary()

generation %>%
  ggplot(aes(x = daily_yield))+
  geom_histogram()

generation %>%
  filter(daily_yield > 0.0) %>%
  ggplot(aes(x = daily_yield))+
  geom_histogram()

generation %>%
  summarize(no_daily_yield_rate = mean(daily_yield == 0)) # 0.222

# Total Yield
generation %>% 
  pull(total_yield) %>% 
  summary()

generation %>%
  ggplot(aes(x = total_yield))+
  geom_histogram()

generation %>%
  filter(total_yield > 0.3) %>%
  ggplot(aes(x = total_yield))+
  geom_histogram() +
  scale_x_log10()

generation %>%
  summarize(no_total_yield_rate = mean(total_yield == 0)) # 0.00413

################################################################################
################################### Averages ###################################
################################################################################

# View entire month average by plant
generation %>%
  group_by(date_time, plant_id) %>%
  summarize(avg_dc_power = mean(dc_power)) %>%
  group_by(plant_id) %>%
  ggplot(aes(x = date_time, y = avg_dc_power, color = plant_id)) +
  geom_line()

# view all sources for one day
generation %>%
  filter(date_time < make_date(2020, 05, 16)) %>%
  group_by(date_time, plant_id) %>%
  summarize(avg_dc_power = mean(dc_power)) %>%
  group_by(plant_id) %>%
  ggplot(aes(x = date_time, y = avg_dc_power, fill = plant_id)) +
  geom_area(position = "stack") +
  theme(legend.position = "none")


# view one day from one source
generation %>%
  filter(source_key == unique(source_key)[1]) %>%
  filter(date_time < make_date(2020, 05, 16)) %>%
  ggplot(aes(x = date_time, y = dc_power)) +
  geom_area()


################################################################################
######################       Weather Data     ##################################
################################################################################

# Ambient Temperature
weather %>%
  pull(ambient_temperature) %>%
  summary()

weather %>%
  ggplot(aes(x = ambient_temperature)) +
  geom_histogram(bins = 40)

# Module Temperature
weather %>%
  pull(module_temperature) %>%
  summary()

weather %>%
  ggplot(aes(x = module_temperature)) +
  geom_histogram(bins = 40)

# Irradiation
weather %>%
  pull(irradiation) %>%
  summary()

weather %>%
  ggplot(aes(x = irradiation)) +
  geom_histogram(bins = 40)

weather %>%
  filter(irradiation > 0.00001) %>%
  ggplot(aes(x = irradiation)) +
  geom_histogram(bins = 40)

weather %>%
  summarize(no_irradiation_rate = mean(irradiation == 0)) # 0.438

################################################################################
################################### Averages ###################################
################################################################################

# view entire month from both sources
weather %>%
  group_by(plant_id) %>%
  ggplot(aes(x = date_time, y = ambient_temperature, color = plant_id)) +
  geom_line()

weather %>%
  group_by(plant_id) %>%
  ggplot(aes(x = date_time, y = module_temperature, color = plant_id)) +
  geom_line()

weather %>%
  group_by(plant_id) %>%
  ggplot(aes(x = date_time, y = irradiation, color = plant_id)) +
  geom_line()


################################################################################
################################# CORRELATION ##################################
################################################################################