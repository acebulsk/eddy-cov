# script to qaqc processed EC data from Licor EddyPro using the Mauder and Foken
# 2004 qaqc system of 0 as best to 9 as worst quality data. 
# IMPORTANT - could be less strict and remove precip filtering on days that I
# cleaned off the KH20 and CSAT

# The QAQC procedure followed in this script is as follows 
# 1. Apply the QC filters calculated within Eddy Pro Post Processing following Foken 2004
# 2. Major max/min thresholding across all vars 
# 3. Wet lens filtering 6 hrs after precip are filterd as likely have precip on the lens (taken from Warren Helgasons class EC qc workflow)
# 4. Calculate a sd filter using a rolling window similar to standardized departures used in MSc times and the Waterloo EC workflow.
# TODO apply regular spike detection and flatline detection from CRHM 

library(wxlogR)
library(tidyverse)
library(zoo)
library(psychRomet)
library(CRHMr)
library(plotly)

options(scipen = 999)

# define constants ----
th_hi <- 200
th_low <- -200
eddy_pro_nan <- -9999.000

spike_th_le_hi <- 90
spike_th_le_lo <- 20

data_avg_min <- 15
data_hz <- 20
perc_min_records <- 0.85
record_min <- data_hz * 60 * data_avg_min * perc_min_records
rolling_window <- 11 # i.e. 5 records prior to and after the current record used in the zoo width argument 

precip_avg_int <- (5*60)/data_avg_min
precip_th_hi <- 1 # min threshold to flag as precip in mm 

flag <- NA
bad_flag <- 2 # “2” for fluxes that should be discarded from the results dataset from Foken et al., 2004. 

# TODO try diff sd for removal
n_sd <- 3.5 # number of sd above / below the mean, taken after the U waterloo EC script which they reference is from Aubinet et al "Eddy Covariance" book, pp.68-69

qc_vars <- c("LE",
             "H",
             "wind_speed",
             "Tau",
             "max_wind_speed")

# bring in raw data from eddy pro

ep_files <- c('eddy-pro-cmd/data/output/eddypro_2016_calibrated_pars_full_output_2023-09-01T120102_exp.csv',
              'eddy-pro-cmd/data/output/eddypro_2016_calibrated_pars_full_output_2023-09-01T170105_exp.csv')

ec_df <- purrr::map_dfr(ep_files, read.csv, skip = 1) |> 
  filter(!filename %in% c('not_enough_data', '')) |> 
  mutate(
    datetime = as.POSIXct(paste(date, time), tz = 'Etc/GMT+6'),
    across(DOY:w.h2o_cov, as.numeric)) |> 
  select(datetime, LE, H, wind_speed,
         Tau:qc_h2o_flux, 
         air_temperature, 
         air_pressure, 
         water_vapor_density,
         wind_speed,
         max_wind_speed, 
         wind_dir_mag = wind_dir, 
         used_records) |> 
  mutate(air_temperature = air_temperature - 273.15) |> 
  arrange(datetime) |> 
  distinct()

# Folken et al., 2004 QAQC ----

# apply LiCOR quality flags already calculated using  Foken et al., 2004; Foken
# and Wichura, 1996; Göckede et al., 2008 Mauder and Foken 2004: policy
# described in the documentation of the TK2 Eddy Covariance software that also
# constituted the standard of the CarboEurope IP project and is widely adopted.
# Here, the combined flag attains the value “0” for best quality fluxes, “1” for
# fluxes suitable for general analysis such as annual budgets and “2” for fluxes
# that should be discarded from the results dataset.

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
  select(c(datetime, qc_vars)) |> 
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

# tdew_df <- tdew_filter |>
#   filter(tdew_filter == bad_flag)
# 
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

ec_fltr_th_met_nrec <- ec_fltr_th_met |> left_join(used_records_filter)

ec_fltr_th_met_nrec <- wxlogR::qc_data_filter(ec_fltr_th_met_nrec, qc_vars, 'used_records_filter', bad_flag, NA)

# STDEV check on rolling window ----
lead_window <- list(1:10)
lag_window <- list(-1:-10)

# convert to long format and group by var name to apply same rolling window
# calculations across each group

ec_fltr_th_met_nrec_sd <- ec_fltr_th_met_nrec |>
  select(datetime, all_of(qc_vars)) |> 
  pivot_longer(qc_vars) |> 
  group_by(name) |> 
  mutate(
    lead_count = rollapply(value,  
                           width=lead_window, 
                           FUN = function(x) sum(!is.na(x)),
                           fill=NA),
    lag_count = rollapply(value,  
                          width=lag_window, 
                          FUN = function(x) sum(!is.na(x)),
                          fill=NA),
    lead_count_filter = ifelse(lead_count<4|is.na(lead_count), 0, 1),
    lag_count_filter = ifelse(lag_count<4|is.na(lag_count), 0, 1),
    lead_value_avg = ifelse(lead_count_filter==1, rollapply(value, width = lead_window, 
                   by = 1, 
                   FUN = mean, 
                   na.rm = T,
                   fill = NA,
                   align = "center"), NA),
    lag_value_avg = ifelse(lag_count_filter==1, rollapply(value, width = lag_window, 
                                                      by = 1, 
                                                      FUN = mean, 
                                                      na.rm = T,
                                                      fill = NA,
                                                      align = "center"), flag),
    lead_value_sd = ifelse(lead_count_filter==1, rollapply(value, width = lead_window, 
                                                      by = 1, 
                                                      FUN = sd, 
                                                      na.rm = T,
                                                      fill = NA,
                                                      align = "center"), flag),
    lag_value_sd = ifelse(lag_count_filter==1, rollapply(value, width = lag_window, 
                                                    by = 1, 
                                                    FUN = sd, 
                                                    na.rm = T,
                                                    fill = NA,
                                                    align = "center"), flag)
  ) |> dplyr::ungroup() |> as.data.frame()

# continuing on same idea as above but dont need grouping anymore since we are
# not doing a rolling window calculation

ec_fltr_th_met_nrec_sd$lead_ec_fltr_max <- 
  ec_fltr_th_met_nrec_sd$lead_value_avg + (n_sd * ec_fltr_th_met_nrec_sd$lead_value_sd)
ec_fltr_th_met_nrec_sd$lead_ec_fltr_min <- 
  ec_fltr_th_met_nrec_sd$lead_value_avg - (n_sd * ec_fltr_th_met_nrec_sd$lead_value_sd)
ec_fltr_th_met_nrec_sd$lag_ec_fltr_max <- 
  ec_fltr_th_met_nrec_sd$lag_value_avg + (n_sd * ec_fltr_th_met_nrec_sd$lag_value_sd)
ec_fltr_th_met_nrec_sd$lag_ec_fltr_min <- 
  ec_fltr_th_met_nrec_sd$lag_value_avg - (n_sd * ec_fltr_th_met_nrec_sd$lag_value_sd)

ec_fltr_th_met_nrec_sd$sd_filter <- ifelse((ec_fltr_th_met_nrec_sd$lead_count_filter == 1 & 
                              ec_fltr_th_met_nrec_sd$value < ec_fltr_th_met_nrec_sd$lead_ec_fltr_max &
                              ec_fltr_th_met_nrec_sd$value > ec_fltr_th_met_nrec_sd$lead_ec_fltr_min) & 
                              (ec_fltr_th_met_nrec_sd$lag_count_filter == 1 & 
                                 ec_fltr_th_met_nrec_sd$value < ec_fltr_th_met_nrec_sd$lag_ec_fltr_max &
                                 ec_fltr_th_met_nrec_sd$value > ec_fltr_th_met_nrec_sd$lag_ec_fltr_min)
                              ,0 , bad_flag)

ec_fltr_ <- wxlogR::qc_data_filter(ec_fltr_th_met_nrec_sd, 
                                  'value', 
                                  'sd_filter', 
                                  bad_flag, flag)

plot_sd_removals <- ec_fltr_th_met_nrec_sd |> 
  filter(sd_filter == 2)

# original IQR range of +/- 3.5 standard deviations seems too strict will have
# to reconsider. Sensible heat flux definitely needs to be higher than 3.5, LE
# and TAU could be closer to 3.5 but also needs to be higher. Wind needs to be
# much higher.

ggplot(ec_fltr_th_met_nrec_sd, aes(datetime, value)) +
  geom_line() +
  facet_wrap(~name) +
  geom_point(data = plot_sd_removals, aes(x = datetime), shape = 4, colour = 'red')

ggplotly()


# spike detection ----

names(ec_fltr)

le_num <- 1
h_num <- 2
wind_num <- 4
tau_num <- 5
max_w_num <- 8

## LE ----

ec_fltr |>
  ggplot(aes(datetime, LE)) + geom_line()

plotly::ggplotly()

ec_fltr$le_qc <- ifelse(ec_fltr$LE < -50, 2, 0)

ec_fltr <- wxlogR::qc_data_filter(ec_fltr, 'LE', 'le_qc', bad_flag, flag)


le_spikes_dates <- ec_fltr |> 
  filter(is.na(LE) == F) |> 
  CRHMr::findSpikes(colnum = le_num,
                    threshold = 100,
                    spike_direction = 'hi')

le_spikes_df <- ec_fltr |> 
  filter(datetime %in% le_spikes_dates)

ggplot(ec_fltr, aes(datetime, LE)) +
  geom_line() +
  geom_point(data = le_spikes_df, aes(x = datetime), shape = 4, colour = 'red')

ggplotly()

le_spike <- ec_fltr |> 
  filter(is.na(LE) == F) |> 
  CRHMr::findSpikes(colnum = 1,
                    threshold = -10,
                    spike_direction = 'hi')



# create complete timeseries

complete_datetime <- wxlogR::datetime_seq_full(ec_3m_post_fltr$datetime)


