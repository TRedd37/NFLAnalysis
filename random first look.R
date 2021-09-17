library(nflfastR)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggimage)
library(RCurl)

url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
df.logos <- read.csv(text = url.logo)
scrimmage.plays.summary <- scrimmage.plays.summary %>% 
  left_join(df.logos, by = c("posteam" = "team_code"))

all_games <- fast_scraper_schedules(1999:2020)

regular_season <- all_games %>%
  filter(game_type == "REG")

away_differential <- regular_season %>%
  group_by(season, away_team) %>%
  summarize(AwayPoints = sum(away_score),
            OppAwayPoints = sum(home_score),
            Wins = sum(away_score > home_score),
            Games = n()) %>%
  mutate(awayDifferential = AwayPoints - OppAwayPoints )

home_differential <- regular_season %>%
  group_by(season, home_team) %>%
  summarize(HomePoints = sum(home_score),
            OppHomePoints = sum(away_score),
            Wins = sum( home_score > away_score),
            Games = n()) %>%
  mutate(homeDifferential = HomePoints - OppHomePoints )

season_summary_data <- away_differential %>%
  inner_join(home_differential, 
             by = c(season = "season", away_team = "home_team"),
             suffix = c("Away", "Home")) %>%
  mutate(pointDifferential = homeDifferential + awayDifferential,
         WinPerc = (WinsAway + WinsHome) / (GamesAway + GamesHome)) %>%
  rename(Team = "away_team") %>%
  select(season, Team, pointDifferential, WinPerc) 

p_images <- season_summary_data %>%
  filter(season > 2005) %>%
  inner_join(teams_colors_logos, by = c(Team = "team_abbr")) %>%
  ggplot(aes(x = pointDifferential, y = WinPerc)) +
  geom_image(aes(image = team_logo_espn), size = 0.05) +
  geom_smooth(method = "glm", formula = "y ~ x", se = FALSE, 
              method.args = list(family = 'binomial') ) +
  xlab("Point Differential") +
  scale_y_continuous(name = "Win Percentage", labels = scales::percent)

p_images

p_dots <- season_summary_data %>%
  ggplot(aes(x = pointDifferential, y = WinPerc, color = Team, shape = season)) +
  geom_point() 

model <- glm(WinPerc ~ pointDifferential, family = 'binomial', data = season_summary_data)

predictions <- predict(model, season_summary_data, type = 'response')

plot_data <- season_summary_data %>%
  bind_cols(data.frame(Prediction = predictions)) %>%
  mutate(Residual = WinPerc - Prediction) %>%
  arrange(Residual) %>%
  inner_join(teams_colors_logos, by = c(Team = "team_abbr")) %>%
  ungroup()


plot_data %>%
  ggplot(aes(x = pointDifferential, y = WinPerc)) +
  geom_image(aes(image = team_logo_espn), size = 0.05) +
  geom_smooth(method = "glm", formula = "y ~ x", se = FALSE, 
              method.args = list(family = 'binomial'),
              fullrange = TRUE) +
  scale_x_continuous(name = "Point Differential", limits = c(-325, 325)) +
  scale_y_continuous(name = "Win Percentage", 
                     labels = scales::percent, 
                     limits = c(0, 1))

plot_data %>%
  slice_min(Residual, n = 10) %>%
  bind_rows(plot_data %>%slice_max(Residual, n = 10) ) %>%
  ggplot(aes(x = pointDifferential, y = WinPerc)) +
  geom_image(aes(image = team_logo_espn), size = 0.05) +
  geom_smooth(data = plot_data, method = "glm", formula = "y ~ x", se = FALSE, 
              method.args = list(family = 'binomial'),
              fullrange = TRUE ) +
  scale_x_continuous(name = "Point Differential", limits = c(-325, 325)) +
  scale_y_continuous(name = "Win Percentage", 
                     labels = scales::percent, 
                     limits = c(0, 1)) 

plot_data %>%
  filter(season > 2010) %>%
  ggplot(aes(x = pointDifferential, y = WinPerc)) +
  geom_image(aes(image = team_logo_espn), size = 0.05) +
  geom_smooth(data = plot_data, method = "glm", formula = "y ~ x", se = FALSE, 
              method.args = list(family = 'binomial'),
              fullrange = TRUE ) +
  scale_x_continuous(name = "Point Differential", limits = c(-325, 325)) +
  scale_y_continuous(name = "Win Percentage", 
                     labels = scales::percent, 
                     limits = c(0, 1)) 
