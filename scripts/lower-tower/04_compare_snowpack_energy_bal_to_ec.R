# Compare EC measurements to snowpack energy balance

# H is negative in winter with the atmosphere loosing heat to the cold surface,
# and in summer H is positive gaining heat from the surface. LE is always
# positive, i.e. moist air mixed upwards from surface evaporation

library(tidyverse)
library(plotly)

snow_survey_df <-
  readRDS('../snow-stats/data/processed/fresh_snow_densities_with_ground_partials.rds') |> 
  pull(datetime) |> as.Date(tz = 'Etc/GMT+6') |> unique()

sp_eb <- readRDS('data/energy-balance/snowpack_energy_balance_15min.rds')

sp_eb_dly <- readRDS('data/energy-balance/snowpack_energy_balance_daily.rds') |> 
  select(date, mod_net_rad, obs_net_rad = net_rad)

ec_lt <- readRDS('data/low-tower/low_tower_15min_2021_2023_qc_rough_semifill.rds') |> 
  select(datetime, LE, H)

ec_lt_dly <- readRDS('data/low-tower/low_tower_15min_2021_2023_qc_rough_semifill_daily.rds') |> 
  select(date, LE, H)

ec_lt_dly |> 
  pivot_longer(!date) |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Mean Daily Energy Flux (w m -2)') +
  xlab(element_blank())

ggsave('figs/energy-bal-assessment/low-tower/mean_daily_turbulent_fluxes_low_tower.png', width = 8.5, height = 4)

net_rads <- sp_eb_dly |> 
  pivot_longer(!date) |> 
  rbind(ec_lt_dly  |> 
          mutate(turbulent = H + LE) |> 
          select(date, turbulent) |> pivot_longer(!date))

net_rads |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Mean Daily Energy Flux (w m -2)') +
  xlab(element_blank())

ggsave('figs/energy-bal-assessment/low-tower/mean_daily_net_turbulent_vs_net_rad_tower.png', width = 8.5, height = 4)

ggplotly()

ec_lt |> 
  mutate(date = as.Date(datetime, tz = 'Etc/GMT+6')) |> 
  filter(date > '2022-03-01',
         date < '2022-04-30') |> 
  pivot_longer(c(LE, H)) |> 
  ggplot(aes(value, colour = name)) + 
  geom_histogram()

start_date <- '2021-12-15'
end_date <- '2021-12-25'

net_rads |> 
  filter(date > start_date,
         date < end_date) |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Mean Daily Energy Flux (w m -2)') +
  xlab(element_blank())

ggsave(
  paste0(
    'figs/energy-bal-assessment/low-tower/mean_daily_net_turbulent_vs_net_rad_tower_',
    start_date,
    '_to_',
    end_date,
    '.png'
  ),
  width = 6,
  height = 4
)


start_date <- '2022-03-01'
end_date <- '2022-04-30'

net_rads |> 
  filter(date > start_date,
         date < end_date) |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Mean Daily Energy Flux (w m -2)') +
  xlab(element_blank())

ggsave(
  paste0(
    'figs/energy-bal-assessment/low-tower/mean_daily_net_turbulent_vs_net_rad_tower_',
    start_date,
    '_to_',
    end_date,
    '.png'
  ),
  width = 6,
  height = 4
)

start_date <- '2023-03-01'
end_date <- '2023-04-30'

net_rads |> 
  filter(date > start_date,
         date < end_date) |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Mean Daily Energy Flux (w m -2)') +
  xlab(element_blank())

ggsave(
  paste0(
    'figs/energy-bal-assessment/low-tower/mean_daily_net_turbulent_vs_net_rad_tower_',
    start_date,
    '_to_',
    end_date,
    '.png'
  ),
  width = 6,
  height = 4
)
