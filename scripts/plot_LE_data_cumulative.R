# script to plot after qaqc and some gap filling has been done

library(tidyverse)
# library(plotly)

select_vars <- c('LE')

to_long <- function(from, to, storm_id){
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime, storm_id)
  
  return(out)
}

to_long_dates <- function(from, to, storm_id){
  date <- seq(from, to, 'day')
  
  out <- data.frame(date, storm_id)
  
  return(out)
}

# constants
ls <- 2.845e6/1e6 # MJ kg-1
J_to_MJ <- 1e-6 # multiply J by this to get MJ


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

# comes in as subl rate mm/15 min (interval)
ffr_mod_subl_long <- readRDS('../ablation/data/obs_mod_canopy_load_and_sublimation_cnpy_snow.rds') |> 
  select(datetime, obs_tree_mod_subl_cpy, mod_subl_cpy) |> 
  pivot_longer(!datetime) |> 
  rename(group = name)

ffr_mod_subl_long_per_sec <- ffr_mod_subl_long |> 
  mutate(value = value / (60 * 15))

canopy_snow_periods <- read.csv('../ablation/data/snow_in_canopy_post_snowfall.csv') |>
  filter(quality < 3) |> 
  mutate(
    across(from:to, ~as.POSIXct(.x, tz = 'Etc/GMT+6')),
    storm_id = as.Date(from, tz = 'Etc/GMT+6')) 

canopy_snow_periods_lng <-
  purrr::pmap_dfr(canopy_snow_periods |> select(c(from, to, storm_id)), to_long)

ec_df_per_interval <- 
  readRDS('data/processed/high_low_ec_data_conv_to_mm_subl_and_crhm_mod_dat_instantaneous.rds') |> 
  mutate(value = case_when(
    group == 'high-tower' ~ (value/(60))*30,
    group == 'low-tower' ~ (value/(60))*15,
    group == 'obs_tree_mod_subl_cpy' ~ (value/(60))*15,
    group == 'mod_subl_cpy' ~ (value/(60))*15,
  ),
  date = as.Date(datetime, tz = 'Etc/GMT+6'))  |> 
  left_join(good_ec_periods) |> 
  filter(is.na(storm_id) == F)

ec_df_fill <- ec_df_per_interval |> 
  dplyr::group_by(storm_id, group) |> 
  mutate(value_fill = imputeTS::na_interpolation(value, 'linear', maxgap = 5),
         value_fill = ifelse(is.na(value_fill) == T, 0, value_fill)) |> 
  ungroup()

ec_df_cml <- ec_df_fill |> 
  dplyr::group_by(storm_id, group) |> 
  mutate(value_cml = cumsum(value_fill)) |> 
  mutate(value_cml_zero = value_cml - min(value_cml)) |> 
  ungroup()

summarise_cml_sub <- ec_df_cml |> 
  # filter(storm_id %in% good_storm_ids) |> 
  group_by(storm_id, group) |> 
  summarise(sum(value_fill))

# plot cumulative for all events at onece ---- 

ggplot(ec_df_cml, aes(datetime, value_cml_zero, colour = group)) + 
  geom_line() +
  facet_grid(~storm_id, scales = 'free') +
  ylab('Cumulative Sublimation Rate (mm)')

ggplotly()

ggsave('figs/model-assessment/mod_cumulative_subl_rate_obs_le_flux_select_events_where_ec_has_data.png', width = 15, height = 8.5)
       
# plot upper ec subtracted by lower ec ----

ec_subl_cpy_high <- ec_df_fill |> 
  filter(group == 'high-tower') |> 
  select(datetime, storm_id, value_high_tower = value_fill)

ec_subl_cpy_low <- ec_df_fill |> 
  filter(group == 'low-tower') |> 
  select(datetime, storm_id, value_low_tower = value_fill)

ec_subl_cpy <- left_join(ec_subl_cpy_high, ec_subl_cpy_low, by = c('datetime', 'storm_id')) |>
  group_by(storm_id) |> 
  mutate(
    value_fill = value_high_tower - value_low_tower,
    value_cml = cumsum(value_fill),
    value_cml_zero = value_cml - min(value_cml),
    group = 'ec_subl_cpy') |> 
  select(datetime, storm_id, group, value_cml_zero)

plot_subl_cpy <- ec_df_cml |> 
  select(datetime, storm_id, group, value_cml_zero) |> 
  filter(group %in% c('mod_subl_cpy', 'obs_tree_mod_subl_cpy')) |> 
  rbind(ec_subl_cpy)

ggplot(plot_subl_cpy, aes(datetime, value_cml_zero, colour = group, group = group)) + 
  geom_line() +
  facet_grid(~storm_id, scales = 'free') +
  ylab('Cumulative Sublimation Rate (mm)')

ggplotly()

ggsave('figs/model-assessment/obs_mod_cumulative_subl_rate_select_events_where_ec_has_data.png', width = 15, height = 8.5)
