# script to plot after qaqc and some gap filling has been done

library(tidyverse)
library(plotly)

select_vars <- c('LE')

to_long_dates <- function(from, to, storm_id){
  date <- seq(from, to, 'day')
  
  out <- data.frame(date, storm_id)
  
  return(out)
}

to_long <- function(from, to, storm_id){
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime, storm_id)
  
  return(out)
}

# TODO decide to use periods snow in canopy OR where we have good EC data OR both? 
# good EC periods where the upper and lower systems are producing data
# the lower ec system was checked prior to the start of each period

good_ec_star_end <- read.csv('data/clean_ec_events.csv') |> 
  mutate(
    from = as.Date(from, tz = 'Etc/GMT+6'),
    to = as.Date(to, tz = 'Etc/GMT+6'),
    storm_id = from)

good_ec_periods <-
  purrr::pmap_dfr(good_ec_star_end |> select(c(from, to, storm_id)), to_long_dates)

# check what days we were on site after snowfall to ensure the EC system was cleaned of snow
snow_survey_df <-
  readRDS('../snow-stats/data/processed/fresh_snow_densities_with_ground_partials.rds') 

snow_surv_dates <- sort((as.Date(snow_survey_df$datetime, tz = 'Etc/GMT+6')) |> unique())

# these ones have an associated cleaning 
clean_storm_ids <-
  c('2022-04-05',
    '2023-01-28',
    '2023-02-24',
    '2023-03-14',
    '2023-03-25'
  )

# events classified as good based on if missing data (may not have a cleaning)

good_storm_ids <- 
  c('2021-12-14',
    '2021-12-29',
    '2022-03-16',
    '2022-03-24',
    '2022-04-05',
    '2022-12-01',
    '2022-12-06',
    '2023-01-28',
    '2023-02-24',
    '2023-02-26',
    '2023-02-28',
    '2023-03-14',
    '2023-03-25',
    '2023-03-26',
    '2023-03-28',
    '2023-04-03',
    '2023-04-04',
    '2023-04-13')

# bring in met data for latent heat of vaporization / sublimation calculation

ffr_met <- readRDS('../met-data-processing/data/ffr_crhm_obs_qaqc_gap_fill.rds') |> 
  select(datetime, mid_tree_air_temp = t)

# comes in as subl rate mm/15 min and multiply by 4 to get hourly 
ffr_mod_subl_long <- readRDS('../ablation/data/obs_mod_canopy_load_and_sublimation_cnpy_snow.rds') |> 
  mutate(obs_tree_mod_subl_cpy = obs_tree_mod_subl_cpy * 4, # this is the observed weighed tree multiplied by the dimensionless sublimation rate (s-1)
         mod_subl_cpy = mod_subl_cpy * 4)  |> 
  select(datetime, obs_tree_mod_subl_cpy, mod_subl_cpy) |> 
  pivot_longer(!datetime) |> 
  rename(group = name)

canopy_snow_periods <- read.csv('../ablation/data/snow_in_canopy_post_snowfall.csv') |>
  filter(quality < 3) |> 
  mutate(
    across(from:to, ~as.POSIXct(.x, tz = 'Etc/GMT+6')),
    storm_id = as.Date(from, tz = 'Etc/GMT+6')) 

canopy_snow_periods_lng <-
  purrr::pmap_dfr(canopy_snow_periods |> select(c(from, to, storm_id)), to_long)

ht <- readRDS('data/high-tower/ec_high_tower_30_min_2021_2023_qc_rough_semifill.rds') |> 
  mutate(group = 'high-tower') |> 
  select(datetime, all_of(select_vars), group)

ht |> ggplot(aes(datetime, LE)) + 
  geom_line() +
  ylab('LE (W m -2)') +
  ggtitle('15 metre tower')

ggplotly()

lt <- readRDS('data/low-tower/low_tower_15min_2021_2023_qc_rough_semifill.rds')  |> 
  mutate(group = 'low-tower') |> 
  select(datetime, all_of(select_vars), group)

lt |> ggplot(aes(datetime, LE)) + 
  geom_line() +
  ylab('LE (W m -2)') +
  ggtitle('3 metre tower')

ec_df <- rbind(ht, lt) |> 
  left_join(ffr_met) |> 
  mutate(
    latent_heat = case_when(
      mid_tree_air_temp < 0 ~ psychRomet::latent_heat_sublimation(mid_tree_air_temp),
      TRUE ~ psychRomet::latent_heat_vaporisation(mid_tree_air_temp)
    ),
    LE_mm_sec = (LE)/latent_heat,
         LE_mm_hr = LE_mm_sec * 60 * 60) |> 
  select(datetime, group, value = LE_mm_hr) |> 
  rbind(ffr_mod_subl_long)

saveRDS(ec_df, 'data/processed/high_low_ec_data_conv_to_mm_subl_and_crhm_mod_dat_instantaneous.rds')

ec_df <- ec_df |> 
  mutate(date = as.Date(datetime, tz = 'Etc/GMT+6')) |> 
  left_join(good_ec_periods) 

ec_df |>
  filter(is.na(storm_id) == F) |> 
  ggplot(aes(datetime, value, colour = group)) +
    geom_line() +
  facet_wrap(~storm_id, scales = 'free')

ggsave('figs/model-assessment/obs_mod_subl_rate_periods_canopy_snow_post_precip_all.png', width = 15, height = 8.5)

ggplotly()
