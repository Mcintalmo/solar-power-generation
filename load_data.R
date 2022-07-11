library(tidyverse)
library(lubridate)

# TODO: Separate data into 3 relational databases, only combine for certain contexts 

data_path <- "./data/"

parse_generation_data <- function(file_name, date_format = "ymd HM") {
  read.csv(paste0(data_path, file_name)) %>%
  as_tibble() %>%
  # For some reason, the two files use different date formatting
  mutate(date_time = parse_date_time(DATE_TIME, date_format)) %>%
  select(-DATE_TIME) %>%
  rename(plant_id = PLANT_ID,
         source_key = SOURCE_KEY,
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

parse_weather_data <- function(file_name) {
  read.csv(paste0(data_path, file_name)) %>%
  as_tibble() %>%
  mutate(date_time = parse_date_time(DATE_TIME, ("ymd HMS"))) %>%
  select(-DATE_TIME) %>%
  rename(plant_id = PLANT_ID,
         source_key = SOURCE_KEY,
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

fill_missing_observations <- function(data) {
  sources <- data %>%
    pull(source_key) %>%
    unique()
  
  date_range <- data %>%
    pull(date_time) %>%
    range()
  
  date_times <- seq(from = date_range[1], to = date_range[2], by='15 mins') # lubridate
  
  plant_id <- data %>%
    pull(plant_id) %>%
    unique()
  
  expand_grid(date_times, sources) %>%
    rename(date_time = date_times, source_key = sources) %>%
    add_column(plant_id = plant_id, .after = "date_time") %>%
    full_join(data, by = c("date_time", "plant_id", "source_key"))
}

date_times_and_sources <-
  

plant_1_generation <- 
  parse_generation_data("Plant_1_Generation_Data.csv", "dmy HM") %>%
  fill_missing_observations()


plant_2_generation <- 
  parse_generation_data("Plant_2_Generation_Data.csv", "ymd HMS") %>%
  fill_missing_observations()


generation <- plant_1_generation %>%
  bind_rows(plant_2_generation)


plant_1_weather <- parse_weather_data("Plant_1_Weather_Sensor_Data.csv") %>%
  fill_missing_observations()

plant_2_weather <- parse_weather_data("Plant_2_Weather_Sensor_Data.csv") %>%
  fill_missing_observations()

weather <- plant_1_weather %>%
  bind_rows(plant_2_weather)

rm(data_path, plant_1_generation, plant_2_generation, plant_1_weather, 
   plant_2_weather,parse_generation_data, parse_weather_data, 
   fill_missing_observations)
