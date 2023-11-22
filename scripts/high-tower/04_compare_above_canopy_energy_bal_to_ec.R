# Compare EC measurements to above canopy energy balance

# TODO how to take into acout change in energy in snow pack to do this? 

# H is negative in winter with the atmosphere loosing heat to the cold surface,
# and in summer H is positive gaining heat from the surface. LE is always
# positive, i.e. moist air mixed upwards from surface evaporation

library(tidyverse)
library(plotly)

to_long_dates <- function(from, to, storm_id){
  date <- seq(from, to, 'day')
  
  out <- data.frame(date, storm_id)
  
  return(out)
}

# good EC periods where the upper and lower systems are producing data
# the lower ec system was checked prior to the start of each period

good_ec_star_end <- read.csv('data/clean_ec_events.csv') |> 
  mutate(
    from = as.Date(from, tz = 'Etc/GMT+6'),
    to = as.Date(to, tz = 'Etc/GMT+6'),
    storm_id = from)

good_ec_periods <-
  purrr::pmap_dfr(good_ec_star_end |> select(c(from, to, storm_id)), to_long_dates)

# the nr lite measures the balance of sw/lw in/out 
ffr_nrlite <- readRDS('../met-data-processing/data/waterloo_1000_met_main.rds') |> 
  select(datetime, 
         meas_net_rad_15m = NR_Wm2_Avg, # TOP ~ 15m
         meas_net_rad_2m = NR_Wm2_2_Avg, # BOTTOM ~ 2m
  )

cp_eb_15 <- readRDS('data/energy-balance/above_canopy_energy_balance_15min.rds') 

cp_eb_dly <- readRDS('data/energy-balance/above_canopy_energy_balance_daily.rds') 

nrlite_dly <- readRDS('data/energy-balance/above_canopy_nrlite_daily.rds')

ec_30 <- readRDS('data/high-tower/ec_high_tower_30_min_2021_2023_qc_rough_semifill.rds')
# ec_dly <- readRDS('data/low-tower/low_tower_15min_2021_2023_qc_rough_semifill_daily.rds') |>
#   select(date, LE, H)

# high tower ec measurements
ec_dly <- readRDS('data/high-tower/ec_high_tower_2021_2023_qc_rough_semifill_daily.rds') |> 
  select(date, LE, H)

# met data for assessing why certain days arent working
ffr_crhm_obs <- readRDS('../met-data-processing/data/ffr_crhm_obs_qaqc_gap_fill.rds')

# plot full date range ----

# missing a ton of measurements because the EC data rarely has a full daay of no nans .. could be more lax and compute daily means with rm.na = F and set a manual filter to keep records with at least 90% of measurements
ec_dly |> 
  pivot_longer(!date) |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Mean Daily Energy Flux (w m -2)') +
  xlab(element_blank())

# ggplotly()

energy_fluxes_individual <- cp_eb_dly |> 
  mutate(lw = lw_in - lw_out,
         sw = sw_in - sw_out) |> 
  select(date, lw, sw) |> 
  pivot_longer(!date) |>  
  mutate(group = 'radiation') |> 
  rbind(ec_dly |> 
          pivot_longer(!date) |> mutate(group = 'turbulent fluxes'))



energy_fluxes_individual |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab('Mean Daily Energy Flux (w m -2)') +
  xlab(element_blank())

ggplotly()

net_rads <- cp_eb_dly |> 
  select(date, sim_net_rad = net_rad) |> 
  pivot_longer(!date) |> 
  rbind(ec_dly  |> 
          mutate(net_rad = H + LE) |> 
          select(date, turbulent = net_rad) |> pivot_longer(!date)) |> 
  rbind(nrlite_dly |> pivot_longer(!date))

net_rads |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Mean Daily Energy Flux (w m -2)') +
  xlab(element_blank())


ggplotly()

# plot select range ----

ec_30 |> 
  mutate(date = as.Date(datetime, tz = 'Etc/GMT+6')) |> 
  left_join(good_ec_periods) |> 
  filter(is.na(storm_id) == F) |> 
  pivot_longer(c(LE, H)) |> 
  ggplot(aes(value, colour = name)) + 
  geom_histogram()

energy_fluxes_individual |> 
  left_join(good_ec_periods) |> 
  filter(is.na(storm_id) == F) |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab('Mean Daily Energy Flux (W m -2)') +
  xlab(element_blank()) +
  facet_wrap(~storm_id, scales = 'free')

ggsave('figs/energy-bal-assessment/individual_fluxes_select_events.png', width = 8.5, height = 7)

net_rads |> 
  left_join(good_ec_periods) |> 
  filter(is.na(storm_id) == F) |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Mean Daily Energy Flux (W m -2)') +
  xlab(element_blank()) +
  facet_wrap(~storm_id, scales = 'free') +
  scale_fill_discrete(name = 'Flux type')

ggsave('figs/energy-bal-assessment/net_radiation_turbulent_flux_compare_select_events.png', width = 8.5, height = 7)

net_rads |> 
  left_join(good_ec_periods) |> 
  filter(is.na(storm_id) == F) |> 
  pivot_wider() |> 
  ggplot(aes(nrlite_net_rad, turbulent)) +
  geom_point() +
  geom_abline() +
  ylab('LE + LH (W m -2)') +
  xlab('Net Rad. - NRLITE (W m -2)')

ggsave('figs/energy-bal-assessment/net_rad_nrlite_vs_ec_measurements.png', width = 6, height = 6)

net_rads |> 
  left_join(good_ec_periods) |> 
  filter(is.na(storm_id) == F) |> 
  pivot_wider() |> 
  ggplot(aes(radiative, turbulent)) +
  geom_point() +
  geom_abline() +
  ylab('LE + LH (W m -2)') +
  xlab('Net Rad. - Simulated (W m -2)')

ggsave('figs/energy-bal-assessment/net_rad_simulated_vs_ec_measurements.png', width = 6, height = 6)

met_daily <- ffr_crhm_obs |> 
  pivot_longer(!datetime) |> 
  mutate(date = as.Date(datetime, tz = 'Etc/GMT+6')) |> 
  group_by(date, name) |> 
  summarise(
    count_nans = sum(is.na(value)),
    value = mean(value, na.rm = T)) |> 
  mutate(value = case_when(
    count_nans > (0.1 * (24*4)) ~ NA, # check if have more than 90% of measurements
    TRUE ~ value
  ))|> 
  select(-count_nans) |> 
  pivot_wider()

met_daily |> 
  left_join(good_ec_periods) |> 
  filter(is.na(storm_id) == F) |> 
  pivot_longer(!c(date, storm_id)) |> 
  ggplot(aes(date, value, colour = name)) +
  # geom_bar(stat='identity') +
  geom_line() +
  geom_point() +
  facet_grid(name~storm_id, scales = 'free') +
  ylab('Daily Average Value')

ggsave('figs/energy-bal-assessment/daily_avg_met_select_events.png', width = 16, height = 11)


ffr_crhm_obs |> 
  mutate(date = as.Date(datetime, tz = 'Etc/GMT+6')) |> 
  left_join(good_ec_periods) |> 
  filter(is.na(storm_id) == F) |> 
  pivot_longer(t:p) |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_grid(name~storm_id, scales = 'free') +
  ylab('15 Minute Average Value')

ggsave('figs/energy-bal-assessment/15_min_avg_met_select_events.png', width = 16, height = 11)

cp_eb_15 |>
  left_join(ffr_nrlite) |> 
  select(datetime, simulated = net_rad, nrlite = meas_net_rad_15m) |> 
  mutate(date = as.Date(datetime, tz = 'Etc/GMT+6')) |> 
  left_join(good_ec_periods) |> 
  filter(is.na(storm_id) == F) |> 
  pivot_longer(c(simulated, nrlite)) |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_wrap(~storm_id, scales = 'free') +
  ylab('Net Radiation (W m -2)') 

ggsave('figs/energy-bal-assessment/mod_meas_net_rad_vs_nrlite_above_canopy_select_events.png', width = 16, height = 11)
