# Data

library(tidyverse)
library(tidycovid19)

# Use downloaded data from tidycovid19
df <- download_merged_data(cached = TRUE, silent = TRUE) %>%
  group_by(country) %>%
  mutate(
    # New daily confirmed cases
    new_confirmed = confirmed - lag(confirmed),
    # New daily confirmed cases per capita
    new_confirmed_pc = new_confirmed / population,
    # Cum confirmed cases per capita
    confirmed_pc = confirmed / population,
    
    # New daily deaths
    new_deaths = deaths - lag(deaths),
    # New daily deaths per capita
    new_deaths_pc = new_deaths / population,
    # Cum deaths per capita
    deaths_pc = deaths / population,
    
    # 7 Day moving averages for confirmed, deaths, and per capita
    ave_7d_new_confirmed = rollmean(new_confirmed, 7, na.pad=TRUE, align="right"),
    ave_7d_new_deaths = rollmean(new_deaths, 7, na.pad=TRUE, align="right"),
    ave_7d_new_confirmed_pc = rollmean(new_confirmed_pc, 7, na.pad=TRUE, align="right"),
    ave_7d_new_deaths_pc = rollmean(new_deaths_pc, 7, na.pad=TRUE, align="right")
  ) %>%
  ungroup()

# Make new df for global data for better na and sum handling
global_df <- df %>%
  group_by(date) %>%
  summarize(
    global_confirmed = sum(confirmed, na.rm = TRUE), 
    global_deaths = sum(deaths, na.rm = TRUE),
    global_soc_dist = sum(soc_dist, na.rm = TRUE),
    global_mov_rest = sum(mov_rest, na.rm = TRUE),
    global_lockdown = sum(lockdown, na.rm = TRUE)
  ) %>%
  mutate(
    global_new_confirmed = global_confirmed - lag(global_confirmed),
    global_new_deaths = global_deaths - lag(global_deaths),
    global_ave_7d_new_deaths = rollmean(global_new_deaths, 7, na.pad=TRUE, align="right"),
    global_ave_7d_new_confirmed = rollmean(global_new_confirmed, 7, na.pad=TRUE, align="right")
  )