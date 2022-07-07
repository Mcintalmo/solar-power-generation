library(tidyverse)
library(lubridate)

#load generation and weather data
source("load_data.R")

head(generation)
dim(generation) # 136476      7
head(weather)
dim(weather) # 6441    6


