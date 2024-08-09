# this script loads the 15 metre high EC data into memory. John mentioned one
# correction we can do to this logger processed data is a axis rotation, we are
# waiting on a response from Michelle Reba on this

# TODO should probably be more lax with LE and H filtering 

library(wxlogR)
library(tidyverse)
library(zoo)
library(CRHMr)
library(plotly)
options(scipen = 999)

ec_in <- readRDS('../met-data-processing/data/waterloo_1000_ec_main.rds')
met <- readRDS('../met-data-processing/data/ffr_crhm_modelling_obs.rds')

# define constants ----

flag <- NA
bad_flag <- 2 # “2” for fluxes that should be discarded from the results dataset from Foken et al., 2004. 

data_avg_min <- 30
data_hz <- 20
perc_min_records <- 0.85
record_min <- data_hz * 60 * data_avg_min * perc_min_records

hrs_last_precip_filter <- 8 # hours
precip_avg_int <- (hrs_last_precip_filter*60)/data_avg_min
precip_th_hi <- 1 # min threshold to flag as precip in mm 

th_hi <- 999
th_low <- -999
eddy_pro_nan <- -9999.000

global_le_min <- -40 # on visual inspection all LE below is erroneous
spike_th_le_hi <- 400
# spike_th_le_lo <- 200
# spike_th_le_both <- 200

global_h_min <- -150
spike_th_h_hi <- 600
spike_th_h_both <- 600
spike_th_h_lo <- 600


global_u_star_max <- 5
global_u_star_min <- 0
spike_th_u_star_hi <- 5
spike_th_u_star_both <- 5

global_tau_max <- 10
global_tau_min <- 0
spike_th_tau_hi <- 5
spike_th_tau_both <- 5

ec_sel <- ec_in |> 
  select(datetime,
         LE = LE_irga,
         H = Hs,
         u_star,
         Tau = tau,
         wind_speed = wnd_spd,
         wind_dir_mag = wnd_dir_compass,
         used_records = n_Tot)

qc_vars <- c(
  'LE',
  'H',
  'Tau',
  'u_star',
  'wind_speed',
  'wind_dir_mag'
)

# global qaqc across select vars ----

ec_fltr_long <- ec_sel |> 
  pivot_longer(all_of(qc_vars)) |>
  mutate(qc_maj_th = 
           case_when(
             value == eddy_pro_nan ~ bad_flag,
             value > th_hi ~ bad_flag,
             value < th_low ~ bad_flag,
             TRUE ~ 0)) |> as.data.frame()

ec_fltr_long$value <-
  ifelse(ec_fltr_long$qc_maj_th == bad_flag, NA, ec_fltr_long$value)

ec_fltr_th <- ec_fltr_long |> 
  select(-qc_maj_th) |> 
  pivot_wider() |> 
  select(c(datetime, all_of(qc_vars))) |> 
  as.data.frame()

# ggplot(ec_fltr_long, aes(datetime, value)) +
#   geom_line() +
#   facet_grid(vars(name),
#              scales = 'free')

# Wet Lens Filtering ---- 

# bring in met data for precip analysis 
# determine if there has been precip in the last 6 hrs

precip_filter <- met |>
  select(datetime, precip_inc = p) |>
  # create filtering data frame for if there has been precip in the last 6 hours
  mutate(
    precip_sum_last_6hr = zoo::rollapply(
      precip_inc,
      width = precip_avg_int,
      by = 1,
      FUN = sum,
      fill = NA,
      na.rm = T,
      partial = T,
      align = 'right'
    ),
    precip_filter_6hr = ifelse(precip_sum_last_6hr >= precip_th_hi, 
                               bad_flag, 0),
    precip_filter_6hr = ifelse(is.na(precip_filter_6hr) == T, 0, 
                               precip_filter_6hr)
  ) |>
  select(datetime, precip_sum_last_6hr, precip_filter_6hr)


# visualize the precip filtering (only periods with more than 1 mm are selected )
precip_6hr_df <- precip_filter |>
  filter(precip_filter_6hr == bad_flag)

# ggplot(precip_filter, aes(datetime, precip_sum_last_6hr)) +
#   geom_line() +
#   geom_point(data = precip_6hr_df, aes(x = datetime), shape = 4, colour = 'red')
# 
# ggplotly()

# check if air temp < dewpoint as there could be potential for moisture on the lens

tdew_filter <- met |> 
  mutate(
    e_sat = psychRomet::clausius_clapeyron(T_c = t),
    e_act = psychRomet::actual_vapour_pressure(e_sat, rh/100),
    tdew = psychRomet::dew_point_temp_e_act(e_act),
    tdew_filter = ifelse(t <  tdew, bad_flag, 0),
    tdew_filter = ifelse(is.na(tdew_filter) == T, 0, tdew_filter))

# visualize the tdew filtering

tdew_df <- tdew_filter |>
  filter(tdew_filter == bad_flag)

# ggplot(tdew_filter, aes(datetime, rh)) +
#   geom_line() +
#   geom_point(data = tdew_df, aes(x = datetime), shape = 4, colour = 'red')
# 
# ggplotly()


# join met filters on raw ec data

ec_fltr_th_met <- ec_fltr_th |> 
  left_join(tdew_filter |> select(datetime, tdew_filter)) |> 
  left_join(precip_filter) 

# only filtering the kh20 vars here

ec_fltr_th_met <-
  wxlogR::qc_data_filter(ec_fltr_th_met, qc_vars, 'tdew_filter', bad_flag, NA)
ec_fltr_th_met <-
  wxlogR::qc_data_filter(ec_fltr_th_met, qc_vars,
                         'precip_filter_6hr', bad_flag, NA)

# ggplot(ec_fltr_th_met, aes(datetime, LE)) +
#   geom_line() +
#   geom_point(data = precip_6hr_df, aes(x = datetime, y = 0), shape = 4, colour = 'red')
# 
# ggplotly()

# Used Records Filter ----

# check there are sufficient number of samples in the 15min time block n>85%

used_records_filter <- ec_sel |> 
  mutate(used_records_filter = if_else(used_records < record_min | used_records == -9999, bad_flag, 0)) |> 
  select(datetime, used_records_filter)

global_var_fltrd <- ec_fltr_th_met |> left_join(used_records_filter)

global_var_fltrd <- wxlogR::qc_data_filter(global_var_fltrd, qc_vars, 'used_records_filter', bad_flag, NA)

## LE Latent Heat Flux ----

### variable specific thresholding ----

# global_var_fltrd |>
#   ggplot(aes(datetime, LE)) + geom_line()
# 
# plotly::ggplotly()

global_var_fltrd$le_qc <- ifelse(global_var_fltrd$LE < global_le_min, 2, 0)

global_var_fltrd <- wxlogR::qc_data_filter(global_var_fltrd, 'LE', 'le_qc', bad_flag, flag)

# global_var_fltrd |>
#   ggplot(aes(datetime, LE)) + geom_line()

# plotly::ggplotly()

### spike detection ----

#### initial pass for hi spikes ----

le_fltr <- global_var_fltrd |>
  filter(is.na(LE) == F) |>
  select(datetime, LE)

le_spikes_dates <- CRHMr::findSpikes(
  le_fltr,
  colnum = 1,
  threshold = spike_th_le_hi,
  spike_direction = 'hi'
)

# plotFlags(le_fltr, le_spikes_dates, 1)
# ggplotly()

le_fltr_delete <- CRHMr::deleteSpikes(
  le_fltr,
  colnum = 1,
  threshold = spike_th_le_hi,
  spike_direction = 'hi'
)

le_fltr <- le_fltr_delete |>
  filter(is.na(LE) == F) 

#### repeat hi spikes filter ----

le_spikes_dates <- CRHMr::findSpikes(
  le_fltr,
  colnum = 1,
  threshold = spike_th_le_hi,
  spike_direction = 'hi'
)

# plotFlags(le_fltr, le_spikes_dates, 1)
# ggplotly()

le_fltr_delete <- CRHMr::deleteSpikes(
  le_fltr,
  colnum = 1,
  threshold = spike_th_le_hi,
  spike_direction = 'hi'
)

le_fltr <- le_fltr_delete |>
  filter(is.na(LE) == F) 

#### repeat hi spikes filter 2 ----

le_spikes_dates <- CRHMr::findSpikes(
  le_fltr,
  colnum = 1,
  threshold = spike_th_le_hi,
  spike_direction = 'hi'
)

# plotFlags(le_fltr, le_spikes_dates, 1)
# ggplotly()

le_fltr_delete <- CRHMr::deleteSpikes(
  le_fltr,
  colnum = 1,
  threshold = spike_th_le_hi,
  spike_direction = 'hi'
)

le_fltr <- le_fltr_delete |>
  filter(is.na(LE) == F) 

# commented this out because we normally have outliers above the mean not below
# could add again below after our rolling window filtering

# le_spikes_dates <- le_fltr |>
#   filter(is.na(LE) == F) |>
#   CRHMr::findSpikes(colnum = 1,
#                     threshold = -300,
#                     spike_direction = 'low')
# 
# plotFlags(le_fltr, le_spikes_dates, 1)
# 
# ggplotly()

### STDEV check on rolling window WIDE ----

lead_window <- list(1:288) # around 3 days to round out diurnal cycling
lag_window <- list(-1:-288)

le_sd_spikes_dates <- CRHMr::findSpikesStdevWindow(le_fltr,
                                                   min_frac_records =  3/288,
                                                   colnum = 1,
                                                   lead_window = lead_window,
                                                   lag_window = lag_window,
                                                   number_sd = 10,
                                                   include_start_end = F
                                                    
)

CRHMr::plotFlags(le_fltr, le_sd_spikes_dates, 1)
# 
# ggplotly()

le_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(le_fltr,
                                                  min_frac_records =  3/288,
                                                  colnum = 1,
                                                  lead_window = lead_window,
                                                  lag_window = lag_window,
                                                  number_sd = 10,
                                                  include_start_end = F)

# ggplot(le_sd_spikes_rm, aes(datetime, LE)) +
#   geom_line()
# 
# ggplotly()

le_sd_spikes_nonan <- le_sd_spikes_rm |>
  select(datetime, LE) |>
  filter(is.na(LE) == F)

### STDEV check on rolling window NARROW ----

# lead_window <- list(1:5)
# lag_window <- list(-1:-5)
# 

# 
# le_sd_spikes_dates <- CRHMr::findSpikesStdevWindow(le_sd_spikes_nonan,
#                                                    min_frac_records =  3/5,
#                                                    colnum = 1,
#                                                    lead_window = lead_window,
#                                                    lag_window = lag_window,
#                                                    number_sd = 30,
#                                                    include_start_end = F
# )
# 
# CRHMr::plotFlags(le_sd_spikes_nonan, le_sd_spikes_dates, 1)
# 
# # ggplotly()
# 
# le_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(le_sd_spikes_nonan,
#                                                   min_frac_records =  3/5,
#                                                   colnum = 1,
#                                                   lead_window = lead_window,
#                                                   lag_window = lag_window,
#                                                   number_sd = 10,
#                                                   include_start_end = F)
# le_sd_spikes_nonan <- le_sd_spikes_rm |>
#   select(datetime, LE) |>
#   filter(is.na(LE) == F)
# 
# le_sd_spikes_dates <- CRHMr::findSpikesStdevWindow(le_sd_spikes_nonan,
#                                                    min_frac_records =  3/5,
#                                                    colnum = 1,
#                                                    lead_window = lead_window,
#                                                    lag_window = lag_window,
#                                                    number_sd = 9,
#include_start_end = F
# )
# 
# CRHMr::plotFlags(le_sd_spikes_nonan, le_sd_spikes_dates, 1)
# 
# # ggplotly()
# 
# # repeat again to catch any doubles that are side by side
# 
# le_sd_spikes_nonan <- CRHMr::deleteSpikesStdevWindow(le_sd_spikes_nonan,
#                                          min_frac_records =  3/5,
#                                          colnum = 1,
#                                          lead_window = lead_window,
#                                          lag_window = lag_window,
#                                        number_sd = 9,
#include_start_end = F)
# ggplot(le_out, aes(datetime, LE)) +
#   geom_line()

# ggplotly()

### manual removal ----

# ggplot(le_sd_spikes_rm, aes(datetime, LE)) +
#   geom_line()
# 
# ggplotly()

bad_times <- c(
  seq(from = as.POSIXct('2021-10-22 12:00', tz = 'Etc/GMT+6'), 
      to = as.POSIXct('2021-10-24 07:30', tz = 'Etc/GMT+6'),
      by = 60*30),
  seq(from = as.POSIXct('2021-12-23 06:30', tz = 'Etc/GMT+6'), 
      to = as.POSIXct('2021-12-27 11:30', tz = 'Etc/GMT+6'),
      by = 60*30),
  seq(from = as.POSIXct('2022-04-08 14:00', tz = 'Etc/GMT+6'), 
      to = as.POSIXct('2022-04-09 20:30', tz = 'Etc/GMT+6'),
      by = 60*30),
  seq(from = as.POSIXct('2022-04-23 01:30', tz = 'Etc/GMT+6'), 
      to = as.POSIXct('2022-04-23 21:30', tz = 'Etc/GMT+6'),
      by = 60*30),
  as.POSIXct('2022-04-30 21:30', tz = 'Etc/GMT+6'),
  as.POSIXct('2023-02-10 09:30', tz = 'Etc/GMT+6'),
  as.POSIXct('2023-03-11 00:30', tz = 'Etc/GMT+6'),
  as.POSIXct('2023-04-24 22:00', tz = 'Etc/GMT+6'))
  
le_out <- le_sd_spikes_nonan |> filter(!datetime %in% bad_times)

# ggplot(le_out, aes(datetime, LE)) +
#   geom_line()

# ggplotly()

## H Sensible Heat Flux ----

### variable specific thresholding ----

h_df <- global_var_fltrd |>
  select(datetime, H)

# global_var_fltrd |>
#   ggplot(aes(datetime, LE)) + geom_line()
# 
# plotly::ggplotly()

h_df$h_qc <- ifelse(h_df$H < global_h_min, 2, 0)

h_df <- wxlogR::qc_data_filter(h_df, 'H', 'h_qc', bad_flag, flag)

h_df_fltr <- h_df |>
  filter(is.na(H) == F) |>
  select(datetime, H)

# h_df_fltr |>
#   ggplot(aes(datetime, H)) + geom_line()

# plotly::ggplotly()

### spike detection ----

#### detect hi and low spikes ----
h_spikes_dates <- CRHMr::findSpikes(
  h_df_fltr,
  colnum = 1,
  threshold = spike_th_h_both,
  spike_direction = 'hi'
)

# plotFlags(h_df_fltr, h_spikes_dates, 1)
# ggplotly()

h_df_fltr <- CRHMr::deleteSpikes(
  h_df_fltr,
  colnum = 1,
  threshold = spike_th_h_both,
  spike_direction = 'hi'
)

h_df_fltr <- h_df_fltr |>
  filter(is.na(H) == F)

#### detect hi spikes ----

h_spikes_dates <- CRHMr::findSpikes(
  h_df_fltr,
  colnum = 1,
  threshold = spike_th_h_hi,
  spike_direction = 'hi'
)

# plotFlags(h_df_fltr, h_spikes_dates, 1)
# ggplotly()

h_df_fltr <- CRHMr::deleteSpikes(
  h_df_fltr,
  colnum = 1,
  threshold = spike_th_h_hi,
  spike_direction = 'hi'
)

h_df_fltr <- h_df_fltr |>
  filter(is.na(H) == F)

### STDEV check on rolling window WIDE ----
wide_window_length <- 288/2
wide_sd <- 10

lead_window <- list(1:wide_window_length) # around 3 days to round out diurnal cycling
lag_window <- list(-1:-wide_window_length)

h_spike_dates <- CRHMr::findSpikesStdevWindow(h_df_fltr, 
                                              min_frac_records =  3/wide_window_length,
                                              colnum = 1,
                                              lead_window = lead_window,
                                              lag_window = lag_window,
                                              number_sd = wide_sd,
                                              include_start_end = F
)

# CRHMr::plotFlags(h_df_fltr, h_spike_dates, 1)

# ggplotly()

h_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(h_df_fltr, 
                                                 min_frac_records =  3/wide_window_length,
                                                 colnum = 1,
                                                 lead_window = lead_window,
                                                 lag_window = lag_window,
                                                 number_sd = wide_sd,
                                                 include_start_end = F)

### STDEV check on rolling window MEDIUM ----

medium_window_length <- 100
medium_sd <- 10

lead_window <- list(1:medium_window_length)
lag_window <- list(-1:-medium_window_length)

h_df_fltr <- h_sd_spikes_rm |> 
  filter(is.na(H) == F)

h_spike_dates <- CRHMr::findSpikesStdevWindow(h_df_fltr, 
                                              min_frac_records =  3/medium_window_length,
                                              colnum = 1,
                                              lead_window = lead_window,
                                              lag_window = lag_window,
                                              number_sd = medium_sd,
                                              include_start_end = F
)

# CRHMr::plotFlags(h_df_fltr, h_spike_dates, 1)

# ggplotly()

h_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(h_df_fltr, 
                                                 min_frac_records =  3/medium_window_length,
                                                 colnum = 1,
                                                 lead_window = lead_window,
                                                 lag_window = lag_window,
                                                 number_sd = medium_sd,
                                                 include_start_end = F)
h_df_fltr <- h_sd_spikes_rm |> 
  filter(is.na(H) == F)

### STDEV check on rolling window NARROW ----

narrow_window_length <- 50
narrow_sd <- 12

lead_window <- list(1:narrow_window_length)
lag_window <- list(-1:-narrow_window_length)

h_df_fltr <- h_sd_spikes_rm |> 
  filter(is.na(H) == F)

h_spike_dates <- CRHMr::findSpikesStdevWindow(h_df_fltr, 
                                              min_frac_records =  3/narrow_window_length,
                                              colnum = 1,
                                              lead_window = lead_window,
                                              lag_window = lag_window,
                                              number_sd = narrow_sd,
                                              include_start_end = F
)

# CRHMr::plotFlags(h_df_fltr, h_spike_dates, 1)

# ggplotly()

h_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(h_df_fltr, 
                                                 min_frac_records =  3/narrow_window_length,
                                                 colnum = 1,
                                                 lead_window = lead_window,
                                                 lag_window = lag_window,
                                                 number_sd = narrow_sd,
                                                 include_start_end = F)
h_df_fltr <- h_sd_spikes_rm |> 
  filter(is.na(H) == F)

### manual removal ----

# ggplot(h_df_fltr, aes(datetime, H)) +
#   geom_line()
# 
# ggplotly()

bad_times <- c(
  seq(from = as.POSIXct('2022-06-12 20:30', tz = 'Etc/GMT+6'), 
      to = as.POSIXct('2022-06-15 08:30', tz = 'Etc/GMT+6'),
      by = 60*30),
  seq(from = as.POSIXct('2023-03-28 09:00', tz = 'Etc/GMT+6'), 
      to = as.POSIXct('2023-03-28 10:30', tz = 'Etc/GMT+6'),
      by = 60*30),
  seq(from = as.POSIXct('2023-04-09 22:00', tz = 'Etc/GMT+6'), 
      to = as.POSIXct('2023-04-10 05:30', tz = 'Etc/GMT+6'),
      by = 60*30))

h_out <- h_df_fltr |> 
  filter(!datetime %in% bad_times)

# ggplot(h_out, aes(datetime, H)) +
#   geom_line()
# 
# ggplotly()

## U star Friction Velocity ----

### variable specific thresholding ----

u_star_df <- global_var_fltrd |> 
  select(datetime, u_star) 

# u_star_df |>
#   ggplot(aes(datetime, u_star)) + geom_line()
# 
# plotly::ggplotly()

u_star_df$u_star_qc <- 0
u_star_df$u_star_qc <- ifelse(u_star_df$u_star < global_u_star_min, bad_flag, u_star_df$u_star_qc)
u_star_df$u_star_qc <- ifelse(u_star_df$u_star > global_u_star_max, bad_flag, u_star_df$u_star_qc)

u_star_df <- wxlogR::qc_data_filter(u_star_df, 'u_star', 'u_star_qc', bad_flag, flag)

u_star_out <- u_star_df |>
  filter(is.na(u_star) == F) |>
  select(datetime, u_star)

# ggplot(u_star_out, aes(datetime, u_star)) + geom_line()
# 
# ggplotly()

### spike detection ----

# commented this out after trying some lax filters which still removed good data 

# u_star_spikes_dates <- CRHMr::findSpikes(
#   u_star_df_fltr,
#   colnum = 1,
#   threshold = .6,
#   spike_direction = 'both'
# )
# 
# plotFlags(u_star_df_fltr, u_star_spikes_dates, 1)
# ggplotly()
# 
# u_star_fltr_delete <- CRHMr::deleteSpikes(
#   u_star_df_fltr,
#   colnum = 1,
#   threshold = 0.5,
#   spike_direction = 'both'
# )
# 
# u_star_df_fltr <- u_star_fltr_delete |>
#   filter(is.na(u_star) == F) 


### STDEV check on rolling window WIDE ----
# removed as no obvious outliers detected here

### STDEV check on rolling window NARROW ----

# removed as no obvious outliers detected here

### manual removal ----

# scanned through all 2 yrs of ustar data no obvious data to manually remove. 

## tau shear stress ----

### variable specific thresholding ----

tau_df <- global_var_fltrd |> 
  select(datetime, Tau) 

# tau_df |>
#   ggplot(aes(datetime, Tau)) + geom_line()
# 
# plotly::ggplotly()

tau_df$tau_qc <- 0
tau_df$tau_qc <- ifelse(tau_df$Tau < global_tau_min, bad_flag, tau_df$tau_qc)
tau_df$tau_qc <- ifelse(tau_df$Tau > global_tau_max, bad_flag, tau_df$tau_qc)

tau_df <- wxlogR::qc_data_filter(tau_df, 'Tau', 'tau_qc', bad_flag, flag)

tau_out <- tau_df |>
  filter(is.na(Tau) == F) |>
  select(datetime, Tau)

# ggplot(tau_out, aes(datetime, Tau)) + geom_line()
# 
# ggplotly()

### spike detection ----

# commented this out after trying some lax filters which still removed good data 

# tau_spikes_dates <- CRHMr::findSpikes(
#   tau_df_fltr,
#   colnum = 1,
#   threshold = .6,
#   spike_direction = 'both'
# )
# 
# plotFlags(tau_df_fltr, tau_spikes_dates, 1)
# ggplotly()
# 
# tau_fltr_delete <- CRHMr::deleteSpikes(
#   tau_df_fltr,
#   colnum = 1,
#   threshold = 0.5,
#   spike_direction = 'both'
# )
# 
# tau_df_fltr <- tau_fltr_delete |>
#   filter(is.na(Tau) == F) 


### STDEV check on rolling window WIDE ----
# removed as no obvious outliers detected here

### STDEV check on rolling window NARROW ----

# removed as no obvious outliers detected here

### manual removal ----

# scanned through all 2 yrs of ustar data no obvious data to manually remove. 

## Wind Speed ----

### variable specific thresholding ----

wnd_df <- global_var_fltrd |> 
  select(datetime, wind_speed) 

mid_tree_wind <- met |> select(datetime, wind_speed = u) |> 
  mutate(group = 'mid_tree')

# rbind(wnd_df |> mutate(group = 'high_tower'), mid_tree_wind) |>
#   ggplot(aes(datetime, wind_speed, colour = group)) + geom_line()
# 
# plotly::ggplotly()

### spike detection ----

# commented this out after trying some lax filters which still removed good data 

wnd_df_fltr <- wnd_df |>
  filter(is.na(wind_speed) == F)

# wnd_spikes_dates <- CRHMr::findSpikes(
#   wnd_df_fltr,
#   colnum = 1,
#   threshold = 4,
#   spike_direction = 'hi'
# )
# 
# plotFlags(wnd_df_fltr, wnd_spikes_dates, 1)
# ggplotly()
# 
# wnd_df_fltr <- CRHMr::deleteSpikes(
#   wnd_df_fltr,
#   colnum = 1,
#   threshold = 2,
#   spike_direction = 'hi'
# )
# 
# wnd_df_fltr <- wnd_df_fltr |>
#   filter(is.na(wind_speed) == F)

### find flatlines ----

flatlines <- findFlatLines(wnd_df_fltr, 1, window_size = 4, 
                           logfile = 'logs/CRHMr_low_ec_wind_flats.log')

### manual removal ----
# 
# ggplot(tau_sd_spikes_rm, aes(datetime, tau)) +
#   geom_line()
# 
# ggplotly()

manual_spike_dates <- c(seq(from = as.POSIXct('2022-03-04 16:00', tz = 'Etc/GMT+6'), 
                            to = as.POSIXct('2022-03-09 12:15', tz = 'Etc/GMT+6'),
                            by = 60*15))

wnd_out <- wnd_df_fltr |> 
  filter(!datetime %in% manual_spike_dates)

# ggplot(wnd_out, aes(datetime, wind_speed)) +
#   geom_line()
# 
# ggplotly()

## wind direction ---- 

wnd_dir_df <- global_var_fltrd |> 
  select(datetime, wind_dir_mag) |> 
  left_join(wnd_out)

wnd_dir_out <- wnd_dir_df |> 
  mutate(wind_dir_mag = case_when(
    is.na(wind_speed) == T ~ NA,
    TRUE ~ wind_dir_mag
  )) |> 
  select(datetime, wind_speed, wind_dir_mag)

sum(is.na(wnd_dir_out$wind_speed))
sum(is.na(wnd_dir_out$wind_dir_mag))

wnd_dir_out <- wnd_dir_out |> 
  select(datetime, wind_dir_mag)

# write data out ----

complete_datetime <- wxlogR::datetime_seq_full(ec_in$datetime)

complete_df <- data.frame(datetime = complete_datetime)

ec_clean_out <- complete_df |> 
  left_join(le_out) |> 
  left_join(h_out) |> 
  left_join(u_star_out) |> 
  left_join(tau_out) |> 
  left_join(wnd_out) |> 
  left_join(wnd_dir_out)

saveRDS(ec_clean_out, 
        'data/high-tower/ec_high_tower_30_min_2021_2023_qc_rough.rds')
