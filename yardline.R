library(dplyr)
library(tidyverse)
library(nflfastR)
data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))


data %>% select(yardline_100, drive, drive_start_yard_line) %>% head()

data[1:5, "yardline_100"]

data %>% head()

data %>%
  head(1000) %>%
  group_by(drive, game_id) %>%
  slice(1, n())

data %>%
  filter(drive == 10, game_id == "2019_01_ATL_MIN")

data %>% head()
data %>% select(total_home_epa, total_away_epa, epa) %>% head()
data %>% colnames()

cleaned_data <- data %>%
  group_by(game_id) %>%
  slice(n()) %>%
  ungroup() %>%
  select(home_team, away_team, total_home_epa, home_score, away_score) %>%
  mutate(score_diff = home_score - away_score,
         epa_diff = total_home_epa - score_diff)

plot(density(cleaned_data$epa_diff))
mean(cleaned_data$epa_diff)



