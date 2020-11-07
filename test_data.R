library(tidyverse)
library(readxl)
library(readr)
library(janitor)


x <- read_excel("raw_data/games-features.xlsx") %>%
  clean_names()

read_excel("raw_data/games-features.xlsx") %>%
  clean_names() %>%
  write_rds("Final_Project/steam_data_set.rds")


x %>%
  filter(metacritic > 0) %>%
  filter(steam_spy_players_estimate > 0) %>%
  filter(steam_spy_players_estimate < 4000000) %>%
  ggplot(aes(x = metacritic, y = steam_spy_players_estimate)) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Estimated Players by Metacritic Scores", x = "Metacritic Score", 
       y = "Estimated Playerbase", subtitle = "Do higher scores mean more players?") +
  theme_light()
  


x %>%
  filter(steam_spy_players_estimate > 0) %>%
  filter(steam_spy_players_estimate < 40000000) %>%
  ggplot(aes(x = steam_spy_players_estimate)) +
  geom_histogram()

