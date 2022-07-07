library(tidyverse)
library(lubridate)

data_path <- "./data/"

parse_generation_data <- function(file_name) {
  read.csv(paste0(data_path, file_name)) %>%
  as_tibble() %>%
  # For some reason, the two files use different date formatting
  mutate(date_time = parse_date_time(DATE_TIME, c("dmy HM", "ymd HMS")),
         plant_id = factor(PLANT_ID),
         source_key = factor(SOURCE_KEY)) %>%
  select(-DATE_TIME, -PLANT_ID, -SOURCE_KEY) %>%
  rename(dc_power = DC_POWER,
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
  mutate(date_time = parse_date_time(DATE_TIME, ("ymd HMS")),
         plant_id = factor(PLANT_ID),
         source_key = SOURCE_KEY) %>%
  select(-DATE_TIME, -PLANT_ID, -SOURCE_KEY) %>%
  rename(ambient_temperature = AMBIENT_TEMPERATURE,
         module_temperature = MODULE_TEMPERATURE,
         irradiation = IRRADIATION) %>%
  relocate(date_time,
           plant_id,
           source_key,
           ambient_temperature,
           module_temperature,
           irradiation)
}

generation <- bind_rows(parse_generation_data("Plant_1_Generation_Data.csv"),
                        parse_generation_data("Plant_2_Generation_Data.csv"))


weather <- bind_rows(parse_weather_data("Plant_1_Weather_Sensor_Data.csv"),
                     parse_weather_data("Plant_2_Weather_Sensor_Data.csv"))

rm(data_path, parse_generation_data, parse_weather_data)
