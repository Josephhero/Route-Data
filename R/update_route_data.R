library(tidyverse)
library(nflreadr)
library(nflplotR)

# Data-----

options(scipen=999)

YEAR <- get_current_season()

players <- load_players() |> 
  filter(position_group %in% c("WR", "TE", "RB", "QB")) |> 
  mutate(name_label = paste0(display_name, " (", position_group, ", #", jersey_number, ")")) |> 
  select(gsis_id, display_name, name_label) 


dat <- load_participation(seasons = YEAR, include_pbp = TRUE) |> 
  filter(!is.na(route)) |> 
  mutate(route = case_when(
    route %in% c("ANGLE", "WHEEL", "SCREEN") ~ "OTHER", 
    TRUE ~ route
  ))

receiver_routes <- dat |> 
  group_by(season, nflverse_game_id, home_team, away_team, week, route, receiver_id, receiver) |> 
  summarize(team_abbr = last(posteam, na_rm = TRUE), 
            week_targets = n(), 
            week_receptions = sum(complete_pass, na.rm = TRUE), 
            week_yards = sum(receiving_yards, na.rm = TRUE), 
            week_yac = sum(yards_after_catch, na.rm = TRUE), 
            week_total_epa = sum(epa, na.rm = TRUE), 
            week_total_air_epa = sum(air_epa, na.rm = TRUE),
            week_total_yac_epa = sum(yac_epa, na.rm = TRUE), 
            week_first_downs = sum(first_down, na.rm = TRUE), 
            week_touchdowns = sum(touchdown, na.rm = TRUE), 
            week_total_cpoe = sum(cpoe, na.rm = TRUE), 
            week_total_success = sum(success, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  left_join(players, by = c("receiver_id" = "gsis_id")) |> 
  rename(receiver_name = display_name, receiver_name_label = name_label) |> 
  select(season, nflverse_game_id, home_team, away_team, week, receiver_id, receiver, 
         receiver_name, receiver_name_label, route, everything())

receiver_list <- receiver_routes |> 
  select(receiver_id, receiver, receiver_name, receiver_name_label) |> 
  distinct()

passer_routes <- dat |> 
  group_by(season, nflverse_game_id, home_team, away_team, week, route, passer_id, passer) |> 
  summarize(team_abbr = last(posteam, na_rm = TRUE), 
            week_passes = n(), 
            week_completions = sum(complete_pass, na.rm = TRUE), 
            week_yards = sum(receiving_yards, na.rm = TRUE), 
            week_yac = sum(yards_after_catch, na.rm = TRUE), 
            week_total_epa = sum(epa, na.rm = TRUE), 
            week_total_air_epa = sum(air_epa, na.rm = TRUE),
            week_total_yac_epa = sum(yac_epa, na.rm = TRUE), 
            week_first_downs = sum(first_down, na.rm = TRUE), 
            week_touchdowns = sum(touchdown, na.rm = TRUE), 
            week_total_cpoe = sum(cpoe, na.rm = TRUE), 
            week_total_success = sum(success, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  left_join(players, by = c("passer_id" = "gsis_id")) |> 
  rename(passer_name = display_name, passer_name_label = name_label) |> 
  select(season, nflverse_game_id, home_team, away_team, team_abbr, week, passer_id, passer, 
         passer_name, passer_name_label, route, everything())
  

passer_list <- passer_routes |> 
  select(passer_id, passer, passer_name, passer_name_label) |> 
  distinct()

seasons <- unique(passer_routes$season)

for (i in seasons){
  
  p_routes <- passer_routes |> 
    filter(season == i)
  
  saveRDS(p_routes, paste0("Data/", i, "_passer_routes.rds"))
  saveRDS(passer_list, paste0("Data/passer_names.rds"))
  
  
}

for (i in seasons){
  
  r_routes <- passer_routes |> 
    filter(season == i)
  
  saveRDS(r_routes, paste0("Data/", i, "_receiver_routes.rds"))
  saveRDS(passer_list, paste0("Data/receiver_names.rds"))
  
}



