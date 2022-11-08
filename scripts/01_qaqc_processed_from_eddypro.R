# script to qaqc processed EC data from Licor EddyPro
# using the Mauder and Foken 2004 qaqc system of 0 as best to 9 as worst quality data.

library(wxlogR)
library(tidyverse)
library(zoo)
library(psychRomet)

# constants 
th_hi <- 200
th_low <- -200

data_avg_min <- 15
data_hz <- 20
perc_min_records <- 0.85
record_min <- data_hz * 60 * data_avg_min * perc_min_records

precip_avg_int <- (5*60)/data_avg_min
precip_th_hi <- 1 # min threshold to flag as precip in mm 

# bring in raw data from eddy pro

ec_raw_compute <- read.csv('data/eddy_pro_output/eddypro_1_full_output_2022-11-04T201154_exp.csv', skip = 1)

raw_units <- as.character(ec_raw_compute[1,])

ec_3m_post <- ec_raw_compute[2:nrow(ec_raw_compute),] 

ec_3m_post <- ec_3m_post |> 
  mutate(
    TIMESTAMP = as.POSIXct(paste(date, time), tz = 'GMT-6'),
    across(DOY:w.h2o_cov, as.numeric)) |> 
  select(TIMESTAMP, Tau:qc_h2o_flux, 
         air_temperature, 
         air_pressure, 
         water_vapor_density,
         wind_speed,
         max_wind_speed, 
         wind_dir_mag = wind_dir, 
         used_records
  )

# apply LiCOR quality flags already calculated using  Foken et al., 2004; Foken and Wichura, 1996; Göckede et al., 2008
# Mauder and Foken 2004: policy described in the documentation of the TK2 Eddy Covariance software 
# that also constituted the standard of the CarboEurope IP project and is widely adopted. Here, the combined 
# flag attains the value “0” for best quality fluxes, “1” for fluxes suitable for general analysis such as annual 
# budgets and “2” for fluxes that should be discarded from the results dataset.

ec_3m_post_fltr <- ec_3m_post

acceptible_flags <- c(0,1, NA)

flag <- NA

ec_3m_post_fltr <- wxlogR::qc_data_filter(ec_3m_post_fltr, 'Tau', 'qc_Tau', acceptible_flags, flag)

ec_3m_post_fltr <- wxlogR::qc_data_filter(ec_3m_post_fltr, 'H', 'qc_H', acceptible_flags, flag)

ec_3m_post_fltr <- wxlogR::qc_data_filter(ec_3m_post_fltr, 'LE', 'qc_LE', acceptible_flags, flag)

# bring in met data for precip analysis 
# determine if there has been precip in the last 6 hrs

pwl_met <- readRDS('../met-data-processing/data/pwl_met_db.rds')

pwl_precip_filter <- pwl_met |> 
  select(TIMESTAMP = datetime, precip_inc) |> 
# create filtering data frame for if there has been precip in the last 6 hours
  mutate(precip_sum_last_6hr = zoo::rollapply(precip_inc, 
                                            width = precip_avg_int, 
                                            by = 1, 
                                            FUN = sum, 
                                            fill = NA,
                                            na.rm = T, 
                                            partial = T,
                                            align = 'right'),
         precip_filter_6hr = ifelse(precip_sum_last_6hr >= precip_th_hi, 2, 0),
         precip_filter_6hr = ifelse(is.na(precip_filter_6hr) == T, 0, precip_filter_6hr)) |> 
  select(TIMESTAMP, precip_filter_6hr)

# check if air temp < dewpoint as there could be potential for moisture on the lens

ffr_met <- readRDS('../met-data-processing/data/met_main_th.rds')

ffr_tdew_filter <- ffr_met |> 
  mutate(
    e_sat = psychRomet::clausius_clapeyron(T_c = AirTC_towerTop),
    e_sat_t = psychRomet::tetens(T_c = AirTC_towerTop),
    e_act = psychRomet::actual_vapour_pressure(e_sat, RH_towerTop/100),
    tdew_top = psychRomet::dew_point_temp_e_act(e_act),
    tdew_filter = ifelse(AirTC_towerTop <  tdew_top, 2, 0),
    tdew_filter = ifelse(is.na(tdew_filter) == T, 0, tdew_filter)) |> 
  select(TIMESTAMP = datetime, tdew_filter)

# join met filters on raw ec data

ec_3m_post_fltr <- ec_3m_post_fltr |> 
  left_join(ffr_tdew_filter) |> 
  left_join(pwl_precip_filter) 

ec_3m_post_fltr <- wxlogR::qc_data_filter(ec_3m_post_fltr, c('Tau', 'H', 'LE'), 'tdew_filter', acceptible_flags, NA)
ec_3m_post_fltr <- wxlogR::qc_data_filter(ec_3m_post_fltr, c('Tau', 'H', 'LE'), 'precip_filter_6hr', acceptible_flags, NA)

# check there are sufficient number of samples in the 15min time block n>85%

ec_3m_post_fltr$used_records_filter <- if_else(ec_3m_post_fltr$used_records < record_min | ec_3m_post_fltr$used_records == -9999, 2, 0)

ec_3m_post_fltr <- wxlogR::qc_data_filter(ec_3m_post_fltr, c('Tau', 'H', 'LE'), 'used_records_filter', acceptible_flags, NA)

# create complete timeseries

complete_datetime <- wxlogR::datetime_seq_full(ec_3m_post_fltr$TIMESTAMP)

# standard deviation check on rolling window

sd_filter <- ec_3m_post |>

  pivot_longer(-c(TIMESTAMP, used_records))
mutate(

)
