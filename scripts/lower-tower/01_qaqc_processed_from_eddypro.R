# script to qaqc processed EC data from Licor EddyPro using the Mauder and Foken
# 2004 qaqc system of 0 as best to 9 as worst quality data. 
# IMPORTANT - could be less strict and remove precip filtering on days that I
# cleaned off the KH20 and CSAT

# The QAQC procedure followed in this script is as follows 
# 1. Apply the QC filters calculated within Eddy Pro Post Processing following Foken 2004
# 2. Major max/min thresholding across all vars 
# 3. Wet lens filtering 6 hrs after precip are filterd as likely have precip on the lens (taken from Warren Helgasons class EC qc workflow)
# 4. Calculate a sd filter using a rolling window similar to standardized departures used in MSc times and the Waterloo EC workflow.

# TODO should probably be more lax with LE and H filtering 
# TODO look at Reba 2009 methods, they do not mention any details of their QAQC
# tests but just reference the use of the Quality Control Software.. could dig
# into this if needed.
# TODO could also go from 15 min averaging to 10 minute average as in Reba 2009
# or could also try 30 min as used in most other homogeneous studies


library(wxlogR)
library(tidyverse)
library(zoo)
library(psychRomet)
library(CRHMr)
library(plotly)

options(scipen = 999)

# define constants ----

th_hi <- 999
th_low <- -999
eddy_pro_nan <- -9999.000

global_le_min <- -40 # on visual inspection all LE below -40 is erroneous
spike_th_le_hi <- 150
# spike_th_le_lo <- 20
# spike_th_le_both <- 150

global_h_min <- -150
spike_th_h_hi <- 300
spike_th_h_both <- 300
spike_th_h_lo <- 300

global_u_star_max <- 5
global_u_star_min <- 0
spike_th_u_star_hi <- 5
spike_th_u_star_both <- 5

global_tau_max <- 10
global_tau_min <- 0
spike_th_tau_hi <- 5
spike_th_tau_both <- 5

data_avg_min <- 15
data_hz <- 20
perc_min_records <- 0.85
record_min <- data_hz * 60 * data_avg_min * perc_min_records
rolling_window <- 11 # i.e. 5 records prior to and after the current record used in the zoo width argument 

precip_avg_int <- (5*60)/data_avg_min
precip_th_hi <- 1 # min threshold to flag as precip in mm 

flag <- NA
bad_flag <- 2 # “2” for fluxes that should be discarded from the results dataset from Foken et al., 2004. 

le_num <- 1
h_num <- 2
wind_num <- 4
u_star_num <- 5
max_w_num <- 8

qc_vars <- c(
  'LE',
  'H',
  'Tau',
  'u_star',
  'wind_speed',
  'wind_dir_mag'
)

# bring in raw data from eddy pro

# ep_files <- c('eddy-pro-cmd/data/output/eddypro_2016_calibrated_pars_full_output_2023-09-01T120102_exp.csv',
#               'eddy-pro-cmd/data/output/eddypro_2016_calibrated_pars_full_output_2023-09-01T170105_exp.csv')
# 
# ec_df <- purrr::map_dfr(ep_files, read.csv, skip = 1) |>
#   filter(!filename %in% c('not_enough_data', '')) |>
#   select(date,
#          time,
#          used_records,
#          wind_speed,
#          u_star = u.,
#          Tau:qc_h2o_flux,
#          max_wind_speed,
#          wind_dir_mag = wind_dir) |>
#     mutate(
#       datetime = as.POSIXct(paste(date, time), tz = 'Etc/GMT+6'),
#       across(used_records:wind_dir_mag, as.numeric),
#       Tau = -Tau) |>
#   select(datetime, used_records:wind_dir_mag) |>
#   arrange(datetime) |>
#   distinct()

# saveRDS(ec_df, 'data/low-tower/low_tower_15min_2021_2023.rds')

ec_df <- readRDS('data/low-tower/low_tower_15min_2021_2023.rds')

# Folken et al., 2004 QAQC ----

# apply LiCOR quality flags already calculated using  Foken et al., 2004; Foken
# and Wichura, 1996; Göckede et al., 2008 Mauder and Foken 2004: policy
# described in the documentation of the TK2 Eddy Covariance software that also
# constituted the standard of the CarboEurope IP project and is widely adopted.
# Here, the combined flag attains the value “0” for best quality fluxes, “1” for
# fluxes suitable for general analysis such as annual budgets and “2” for fluxes
# that should be discarded from the results dataset.

# we can use the Tau flag to qc u_star because 
# u_star ^ 2 = abs(tau/density of air)
ec_fltr <- wxlogR::qc_data_filter(ec_df, 'u_star', 'qc_Tau', bad_flag, flag)
ec_fltr <- wxlogR::qc_data_filter(ec_df, 'Tau', 'qc_Tau', bad_flag, flag)
ec_fltr <- wxlogR::qc_data_filter(ec_fltr, 'H', 'qc_H', bad_flag, flag)
ec_fltr <- wxlogR::qc_data_filter(ec_fltr, 'LE', 'qc_LE', bad_flag, flag)

# ec_fltr <- wxlogR::qc_data_filter(ec_fltr, 'h2o_flux', 'qc_h2o_flux', bad_flag, flag)

# Major Thresholding ----

ec_fltr_long <- ec_fltr |> 
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

# Wet Lens Filtering ---- 

# bring in met data for precip analysis 
# determine if there has been precip in the last 6 hrs

met <- readRDS('../met-data-processing/data/ffr_crhm_modelling_obs.rds')

precip_filter <- met |> 
  select(datetime, precip_inc = p) |> 
# create filtering data frame for if there has been precip in the last 6 hours
  mutate(precip_sum_last_6hr = zoo::rollapply(precip_inc, 
                                            width = precip_avg_int, 
                                            by = 1, 
                                            FUN = sum, 
                                            fill = NA,
                                            na.rm = T, 
                                            partial = T,
                                            align = 'right'),
         precip_filter_6hr = ifelse(precip_sum_last_6hr >= precip_th_hi, bad_flag, 0),
         precip_filter_6hr = ifelse(is.na(precip_filter_6hr) == T, 0, precip_filter_6hr)) |> 
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

ec_fltr_th_met <- wxlogR::qc_data_filter(ec_fltr_th_met, qc_vars, 'tdew_filter', bad_flag, NA)
ec_fltr_th_met <- wxlogR::qc_data_filter(ec_fltr_th_met, qc_vars, 'precip_filter_6hr', bad_flag, NA)

# ggplot(ec_fltr_th_met, aes(datetime, LE)) +
#   geom_line() +
#   geom_point(data = precip_6hr_df, aes(x = datetime, y = 0), shape = 4, colour = 'red')
# 
# ggplotly()

# check there are sufficient number of samples in the 15min time block n>85%

used_records_filter <- ec_df |> 
  mutate(used_records_filter = if_else(ec_df$used_records < record_min | ec_df$used_records == -9999, bad_flag, 0)) |> 
  select(datetime, used_records_filter)

global_var_fltrd <- ec_fltr_th_met |> left_join(used_records_filter)

global_var_fltrd <- wxlogR::qc_data_filter(global_var_fltrd, qc_vars, 'used_records_filter', bad_flag, NA)

# spike detection ----

# names(ec_fltr)

## LE Latent Heat Flux ----

### variable specific thresholding ----

# global_var_fltrd |>
#   ggplot(aes(datetime, LE)) + geom_line()
# 
# plotly::ggplotly()

global_var_fltrd$le_qc <- ifelse(global_var_fltrd$LE < global_le_min, 2, 0)

global_var_fltrd <- wxlogR::qc_data_filter(global_var_fltrd, 'LE', 'le_qc', bad_flag, flag)

### spike detection ----

le_fltr <- global_var_fltrd |>
  filter(is.na(LE) == F) |>
  select(datetime, LE)

le_spikes_dates <- CRHMr::findSpikes(
  le_fltr,
  colnum = le_num,
  threshold = spike_th_le_hi,
  spike_direction = 'hi'
)

plotFlags(le_fltr, le_spikes_dates, le_num)
ggplotly()

le_fltr_delete <- CRHMr::deleteSpikes(
  le_fltr,
  colnum = le_num,
  threshold = spike_th_le_hi,
  spike_direction = 'hi'
)

# commented this out because we normally have outliers above the mean not below
# could add again below after our rolling window filtering
# le_spikes_dates <- le_fltr_delete |>
#   filter(is.na(LE) == F) |>
#   CRHMr::findSpikes(colnum = le_num,
#                     threshold = -100,
#                     spike_direction = 'low')

# plotFlags(global_var_fltrd, le_spikes_dates, le_num)
#
# ggplotly()

### STDEV check on rolling window WIDE ----

lead_window <- list(1:288) # around 3 days to round out diurnal cycling
lag_window <- list(-1:-288)

le_sd_spikes_nonan <- le_fltr_delete |>
  select(datetime, LE) |>
  filter(is.na(LE) == F)

le_sd_spikes_dates <- CRHMr::findSpikesStdevWindow(le_sd_spikes_nonan,
                                                   min_frac_records =  3/288,
                                                   colnum = le_num,
                                                   lead_window = lead_window,
                                                   lag_window = lag_window,
                                                   number_sd = 10,
                                                   include_start_end = F
                                                   )

CRHMr::plotFlags(le_fltr_delete, le_sd_spikes_dates, le_num)
# 
# ggplotly()

le_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(le_sd_spikes_nonan,
                                                  min_frac_records =  3/288,
                                                  colnum = le_num,
                                                  lead_window = lead_window,
                                                  lag_window = lag_window,
                                                  number_sd = 10,
                                                  include_start_end = F)

# ggplot(le_sd_spikes_rm, aes(datetime, LE)) +
#   geom_line()
# 
# ggplotly()

### STDEV check on rolling window NARROW ----

lead_window <- list(1:5)
lag_window <- list(-1:-5)

le_sd_spikes_nonan <- le_sd_spikes_rm |>
  select(datetime, LE) |>
  filter(is.na(LE) == F)

le_sd_spikes_dates <- CRHMr::findSpikesStdevWindow(le_sd_spikes_nonan,
                                                   min_frac_records =  3/5,
                                                   colnum = le_num,
                                                   lead_window = lead_window,
                                                   lag_window = lag_window,
                                                   number_sd = 9,
                                                   include_start_end = F
)

# CRHMr::plotFlags(le_sd_spikes_nonan, le_sd_spikes_dates, le_num)
# 
# ggplotly()

le_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(le_sd_spikes_nonan,
                                                  min_frac_records =  3/5,
                                                  colnum = le_num,
                                                  lead_window = lead_window,
                                                  lag_window = lag_window,
                                                  number_sd = 9,
                                                  include_start_end = F)
le_sd_spikes_nonan <- le_sd_spikes_rm |>
  select(datetime, LE) |>
  filter(is.na(LE) == F)

le_sd_spikes_dates <- CRHMr::findSpikesStdevWindow(le_sd_spikes_nonan,
                                                   min_frac_records =  3/5,
                                                   colnum = le_num,
                                                   lead_window = lead_window,
                                                   lag_window = lag_window,
                                                   number_sd = 9,
                                                   include_start_end = F
)

# CRHMr::plotFlags(le_sd_spikes_nonan, le_sd_spikes_dates, le_num)
# 
# ggplotly()

# repeat again to catch any doubles that are side by side

le_out <- CRHMr::deleteSpikesStdevWindow(le_sd_spikes_nonan,
                                                  min_frac_records =  3/5,
                                                  colnum = le_num,
                                                  lead_window = lead_window,
                                                  lag_window = lag_window,
                                                  number_sd = 9,
                                         include_start_end = F)

### manual removal ----

# ggplot(le_sd_spikes_rm, aes(datetime, LE)) +
#   geom_line()
# 
# ggplotly()

# TODO update dates below to manually remove any bad values. Will finish this
# when we figure out one time frame we need the LE values for.
# manual_spike_dates <- c( '2021-11-21 01:30', '2021-11-21 01:45', '2021-11-22
# 19:30:00' )

## H Sensible Heat Flux ----

### variable specific thresholding ----

h_df <- global_var_fltrd |>
  select(datetime, H)
# 
# h_df |>
#   ggplot(aes(datetime, H)) + geom_line()
# 
# plotly::ggplotly()

h_df$h_qc <- ifelse(h_df$H < global_h_min, 2, 0)

h_df <- wxlogR::qc_data_filter(h_df, 'H', 'h_qc', bad_flag, flag)

h_df_fltr <- h_df |>
  filter(is.na(H) == F) |>
  select(datetime, H)

### spike detection ----

#### detect hi and low spikes ----
h_spikes_dates <- CRHMr::findSpikes(
  h_df_fltr,
  colnum = 1,
  threshold = spike_th_h_both,
  spike_direction = 'both'
)

# plotFlags(h_df_fltr, h_spikes_dates, 1)
# ggplotly()

h_df_fltr <- CRHMr::deleteSpikes(
  h_df_fltr,
  colnum = 1,
  threshold = spike_th_h_both,
  spike_direction = 'both'
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

lead_window <- list(1:288) # around 3 days to round out diurnal cycling
lag_window <- list(-1:-288)

h_spike_dates <- CRHMr::findSpikesStdevWindow(h_df_fltr, 
                                                   min_frac_records =  3/288,
                                                   colnum = 1,
                                                   lead_window = lead_window,
                                                   lag_window = lag_window,
                                                   number_sd = 8,
                                              include_start_end = F
)

# CRHMr::plotFlags(h_df_fltr, h_spike_dates, 1)
# 
# ggplotly()

h_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(h_df_fltr, 
                                                  min_frac_records =  3/288,
                                                  colnum = 1,
                                                  lead_window = lead_window,
                                                  lag_window = lag_window,
                                                  number_sd = 8,
                                                 include_start_end = F)

### STDEV check on rolling window NARROW ----

lead_window <- list(1:10)
lag_window <- list(-1:-10)

h_df_fltr <- h_sd_spikes_rm |> 
  filter(is.na(H) == F)

h_spike_dates <- CRHMr::findSpikesStdevWindow(h_df_fltr, 
                                                   min_frac_records =  3/10,
                                                   colnum = 1,
                                                   lead_window = lead_window,
                                                   lag_window = lag_window,
                                                   number_sd = 5,
                                              include_start_end = F
)

# CRHMr::plotFlags(h_df_fltr, h_spike_dates, 1)
# 
# ggplotly()

h_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(h_df_fltr, 
                                                  min_frac_records =  3/10,
                                                  colnum = 1,
                                                  lead_window = lead_window,
                                                  lag_window = lag_window,
                                                  number_sd = 5,
                                                 include_start_end = F)
h_df_fltr <- h_sd_spikes_rm |> 
  filter(is.na(H) == F)

lead_window <- list(1:5)
lag_window <- list(-1:-5)

h_spike_dates <- CRHMr::findSpikesStdevWindow(h_df_fltr, 
                                              min_frac_records =  3/5,
                                              colnum = 1,
                                              lead_window = lead_window,
                                              lag_window = lag_window,
                                              number_sd = 10,
                                              include_start_end = F
)

# CRHMr::plotFlags(h_df_fltr, h_spike_dates, le_num)
# 
# ggplotly()

h_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(h_df_fltr, 
                                                 min_frac_records =  3/5,
                                                 colnum = 1,
                                                 lead_window = lead_window,
                                                 lag_window = lag_window,
                                                 number_sd = 10,
                                                 include_start_end = F)

h_df_fltr <- h_sd_spikes_rm |> 
  filter(is.na(H) == F)

### manual removal ----

# ggplot(h_df_fltr, aes(datetime, H)) +
#   geom_line()
# 
# ggplotly()

manual_spike_dates <- c('2022-05-06 06:30:00', 
                        '2022-05-06 07:30:00', 
                        '2021-11-22 19:30:00' ) |> as.POSIXct(tz = 'Etc/GMT+6')

h_out <- h_df_fltr |> 
  filter(!datetime %in% manual_spike_dates)

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

u_star_df_fltr <- u_star_df |>
  filter(is.na(u_star) == F) |>
  select(datetime, u_star)

### spike detection ----

# commented this out after trying some lax filters which still removed good data 

# u_star_spikes_dates <- CRHMr::findSpikes(
#   u_star_df_fltr,
#   colnum = 1,
#   threshold = .5,
#   spike_direction = 'hi'
# )
# 
# # plotFlags(u_star_df_fltr, u_star_spikes_dates, 1)
# # ggplotly()
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

lead_window <- list(1:288) # around 3 days to round out diurnal cycling
lag_window <- list(-1:-288)

u_star_spike_dates <- CRHMr::findSpikesStdevWindow(u_star_df_fltr, 
                                              min_frac_records =  3/288,
                                              colnum = 1,
                                              lead_window = lead_window,
                                              lag_window = lag_window,
                                              number_sd = 8,
                                              include_start_end = F
)

# CRHMr::plotFlags(u_star_df_fltr, u_star_spike_dates, 1)
# 
# ggplotly()

# disable u_star rolling window stdev filtering 

# u_star_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(u_star_df_fltr, 
#                                                  min_frac_records =  3/288,
#                                                  colnum = 1,
#                                                  lead_window = lead_window,
#                                                  lag_window = lag_window,
#                                                  number_sd = 8,
#include_start_end = F)

### manual removal ----

# ggplot(u_star_df, aes(datetime, u_star)) +
#   geom_line()
# 
# ggplotly()

manual_spike_dates <- c('2022-04-23 05:15:00', 
                        '2022-04-23 05:30:00') |> as.POSIXct()

u_star_out <- u_star_df_fltr |> 
  filter(!datetime %in% manual_spike_dates)

# ggplot(u_star_df_fltr_manual, aes(datetime, u_star)) +
#   geom_line()
# 
# ggplotly()

## Tau Shear Stress ----

### variable specific thresholding ----

tau_df <- global_var_fltrd |> 
  select(datetime, tau = Tau) 

# u_star_df |>
#   ggplot(aes(datetime, u_star)) + geom_line()
# 
# plotly::ggplotly()

tau_df$tau_qc <- 0
tau_df$tau_qc <- ifelse(tau_df$tau < global_tau_min, bad_flag, tau_df$tau_qc)
tau_df$tau_qc <- ifelse(tau_df$tau > global_tau_max, bad_flag, tau_df$tau_qc)

tau_df <- wxlogR::qc_data_filter(tau_df, 'tau', 'tau_qc', bad_flag, flag)

tau_df_fltr <- tau_df |>
  filter(is.na(tau) == F) |>
  select(datetime, tau)

### spike detection ----

# commented this out after trying some lax filters which still removed good data 

tau_spikes_dates <- CRHMr::findSpikes(
  tau_df_fltr,
  colnum = 1,
  threshold = 2,
  spike_direction = 'both'
)

# plotFlags(tau_df_fltr, tau_spikes_dates, 1)
# ggplotly()

tau_fltr_delete <- CRHMr::deleteSpikes(
  tau_df_fltr,
  colnum = 1,
  threshold = 2,
  spike_direction = 'both'
)

tau_df_fltr <- tau_fltr_delete |>
  filter(is.na(tau) == F)


### STDEV check on rolling window WIDE ----

lead_window <- list(1:288) # around 3 days to round out diurnal cycling
lag_window <- list(-1:-288)

tau_spike_dates <- CRHMr::findSpikesStdevWindow(tau_df_fltr, 
                                                   min_frac_records =  3/288,
                                                   colnum = 1,
                                                   lead_window = lead_window,
                                                   lag_window = lag_window,
                                                   number_sd = 20,
                                                include_start_end = F
)

# CRHMr::plotFlags(tau_df_fltr, tau_spike_dates, 1)
# 
# ggplotly()

tau_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(tau_df_fltr,
                                                 min_frac_records =  3/288,
                                                 colnum = 1,
                                                 lead_window = lead_window,
                                                 lag_window = lag_window,
                                                 number_sd = 20,
                                                 include_start_end = F)

tau_df_fltr <- tau_sd_spikes_rm |>
  filter(is.na(tau) == F)

### STDEV check on rolling window NARROW ----

lead_window <- list(1:20) 
lag_window <- list(-1:-20)

tau_spike_dates <- CRHMr::findSpikesStdevWindow(tau_df_fltr, 
                                                min_frac_records =  3/20,
                                                colnum = 1,
                                                lead_window = lead_window,
                                                lag_window = lag_window,
                                                number_sd = 12,
                                                include_start_end = F
)

# CRHMr::plotFlags(tau_df_fltr, tau_spike_dates, 1)
# 
# ggplotly()

tau_sd_spikes_rm <- CRHMr::deleteSpikesStdevWindow(tau_df_fltr,
                                                   min_frac_records =  3/288,
                                                   colnum = 1,
                                                   lead_window = lead_window,
                                                   lag_window = lag_window,
                                                   number_sd = 12,
                                                   include_start_end = F)


### manual removal ----

# ggplot(tau_sd_spikes_rm, aes(datetime, tau)) +
#   geom_line()
# 
# ggplotly()

manual_spike_dates <- c(seq(from = as.POSIXct('2021-11-14 17:30', tz = 'Etc/GMT+6'), 
                            to = as.POSIXct('2021-11-14 21:15', tz = 'Etc/GMT+6'),
                            by = 60*30),
                        seq(from = as.POSIXct('2021-11-28 09:15', tz = 'Etc/GMT+6'), 
                            to = as.POSIXct('2021-11-30 07:00', tz = 'Etc/GMT+6'),
                            by = 60*30),
                        seq(from = as.POSIXct('2021-05-28 00:00', tz = 'Etc/GMT+6'), 
                            to = as.POSIXct('2021-05-28 04:45', tz = 'Etc/GMT+6'),
                            by = 60*30))

tau_out <- tau_df_fltr |> 
  filter(!datetime %in% manual_spike_dates)

# ggplot(tau_df_fltr_manual, aes(datetime, tau)) +
#   geom_line()
# 
# ggplotly()

# create complete timeseries

complete_datetime <- wxlogR::datetime_seq_full(ec_df$datetime)

complete_df <- data.frame(datetime = complete_datetime)

ec_clean_out <- complete_df |> 
  left_join(le_out) |> 
  left_join(h_out) |> 
  left_join(u_star_out) |> 
  left_join(tau_out)
  
saveRDS(ec_clean_out, 'data/low-tower/low_tower_15min_2021_2023_qc_rough.rds')

## Wind Speed ----

### variable specific thresholding ----

wnd_df <- global_var_fltrd |> 
  select(datetime, wind_speed) 

mid_tree_wind <- met |> select(datetime, wind_speed = u) |> 
  mutate(group = 'mid_tree')

high_tower_wnd <- readRDS('../met-data-processing/data/waterloo_1000_ec_main.rds') |> select(datetime, wind_speed = wnd_spd) |> 
  mutate(group = 'high_tower')

# rbind(wnd_df |> mutate(group = 'low_tower'), mid_tree_wind) |>
#   rbind(high_tower_wnd) |> 
#   ggplot(aes(datetime, wind_speed, colour = group)) + geom_line()
# 
# plotly::ggplotly()

### spike detection ----

# commented this out after trying some lax filters which still removed good data 

wnd_df_fltr <- wnd_df |>
  filter(is.na(wind_speed) == F)

wnd_spikes_dates <- CRHMr::findSpikes(
  wnd_df_fltr,
  colnum = 1,
  threshold = 2,
  spike_direction = 'hi'
)

# plotFlags(wnd_df_fltr, wnd_spikes_dates, 1)
# ggplotly()

wnd_fltr_delete <- CRHMr::deleteSpikes(
  wnd_df_fltr,
  colnum = 1,
  threshold = 2,
  spike_direction = 'hi'
)

wnd_df_fltr <- wnd_fltr_delete |>
  filter(is.na(wind_speed) == F)

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
  select(datetime, wind_dir_mag)

sum(is.na(wnd_dir_out$wind_speed))
sum(is.na(wnd_dir_out$wind_dir_mag))

# create complete timeseries

complete_datetime <- wxlogR::datetime_seq_full(ec_df$datetime)

complete_df <- data.frame(datetime = complete_datetime)

ec_clean_out <- complete_df |> 
  left_join(le_out) |> 
  left_join(h_out) |> 
  left_join(u_star_out) |> 
  left_join(tau_out) |> 
  left_join(wnd_out) |> 
  left_join(wnd_dir_out)

saveRDS(ec_clean_out, 'data/low-tower/low_tower_15min_2021_2023_qc_rough.rds')



