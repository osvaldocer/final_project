library(tidyverse)
library(readxl)
library(readr)
library(janitor)


read_excel("raw_data/games-features.xlsx") %>%
  clean_names() %>%
  write_rds("Final_Project/steam_data_set.rds")
  


