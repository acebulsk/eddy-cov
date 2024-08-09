# script to fill NaNs using linear interpolation for small gaps

library(tidyverse)
library(plotly)
library(CRHMr)

max_gap_fill_linear <- 2

le_col <- 1
h_col <- 2
u_star_col <- 3
tau_col <- 4


ec_in <- readRDS('data/low-tower/low_tower_15min_2021_2023_qc_rough.rds')

findGaps(ec_in, gapfile = 'crhm-logs/ec_in_gaps.csv')

# can see lots of single tx gaps in the file above so worth proceeding with
# linear interpolation to fill these small gaps

# latent heat ----

le_fill <- CRHMr::interpolate(ec_in, 
                                      varcols = le_col,
                                      methods = 'linear', 
                                      maxlength = max_gap_fill_linear)

le_still_have_gaps <- findGaps(le_fill, gapfile = 'crhm-logs/le_gaps.csv', quiet = F)

if(!le_still_have_gaps == F){
  warning("LE still has gaps.")
}

# sensible heat ----

h_fill <- CRHMr::interpolate(ec_in, 
                              varcols = h_col,
                              methods = 'linear', 
                              maxlength = max_gap_fill_linear)

h_still_have_gaps <- findGaps(h_fill, gapfile = 'crhm-logs/h_gaps.csv', quiet = F)

if(!h_still_have_gaps == F){
  warning("h still has gaps.")
}

# friction velocity ----

u_star_fill <- CRHMr::interpolate(ec_in, 
                              varcols = u_star_col,
                              methods = 'linear', 
                              maxlength = max_gap_fill_linear)

u_star_still_have_gaps <- findGaps(u_star_fill, gapfile = 'crhm-logs/u_star_gaps.csv', quiet = F)

if(!u_star_still_have_gaps == F){
  warning("u_star still has gaps.")
}

# shear stress ----

tau_fill <- CRHMr::interpolate(ec_in, 
                                  varcols = tau_col,
                                  methods = 'linear', 
                                  maxlength = max_gap_fill_linear)

tau_still_have_gaps <- findGaps(tau_fill, gapfile = 'crhm-logs/tau_gaps.csv', quiet = F)

if(!tau_still_have_gaps == F){
  warning("tau still has gaps.")
}

# combine filled dfs and write out ---- 

# note df here still has some larger gaps could consider a station regression
# fill but unsure if this is standard practice with EC data

ec_fill_out <- le_fill |> 
  left_join(h_fill) |> 
  left_join(u_star_fill) |> 
  left_join(tau_fill)

saveRDS(ec_fill_out, 'data/low-tower/low_tower_15min_2021_2023_qc_rough_semifill.rds')

ec_fill_out_daily <- ec_fill_out |> 
  pivot_longer(!datetime) |> 
  mutate(date = as.Date(datetime, tz = 'Etc/GMT+6')) |> 
  group_by(date, name) |> 
  summarise(
    count_nans = sum(is.na(value)),
    value = mean(value, na.rm = T)) |> 
  mutate(value = case_when(
    count_nans > (0.25 * (24*4)) ~ NA, # check if have more than 75% of measurements
    TRUE ~ value
  ))|> 
  select(-count_nans) |>
  pivot_wider()

saveRDS(ec_fill_out_daily, 'data/low-tower/low_tower_15min_2021_2023_qc_rough_semifill_daily.rds')
