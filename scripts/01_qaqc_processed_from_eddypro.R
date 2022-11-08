# script to qaqc processed EC data from Licor EddyPro

library(wxlogR)
library(tidyverse)
library(zoo)
library(psychRomet)

# constants 
p_atm <- psychRomet::pressure_atmosphere(2000) # kpa

th_hi <- 200
th_low <- -200

data_avg_min <- 15
data_hz <- 20
perc_min_records <- 0.85
record_min <- data_hz * 60 * data_avg_min * perc_min_records

precip_avg_int <- (5*60)/data_avg_min
precip_th_hi <- 1 # min threshold to flag as precip in mm 

# bring in met data for precip analysis 
# determine if there has been more than 10 mm precip in the last 6 hrs

pwl_met <- readRDS('../interception/data/pwl/pwl_met_db.rds')

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
         precip_filter_6hr = ifelse(precip_sum_last_6hr >= precip_th_hi, 1, 0))

# check if air temp < dewpoint as there could be potential for moisture on the lens

ffr_met <- readRDS('../interception/data/met/met_main_th.rds')

ffr_tdew_filter <- ffr_met |> 
  mutate(
    e_sat = psychRomet::clausius_clapeyron(T_c = AirTC_towerTop),
    e_act = psychRomet::actual_vapour_pressure(e_sat, RH_towerTop/100),
    tdew_top = psychRomet::dew_point_temp_e_act(e_act),
    tdew_filter = ifelse(AirTC_towerTop <  tdew_top, 1, 0)) |> 
  select(TIMESTAMP = datetime, tdew_filter)


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

used_records_filter <- if_else(ec_3m_post$used_records < record_min, 0, 1)

wxlogR::plot_data_gaps()
