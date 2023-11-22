# Energy Balance for the snowpack below the 3 m EC system for quality assessment
# of EC measurements

library(tidyverse)
library(plotly)

# TODO decide what canopy metrics to use, the footprint of the upper ec system
# seems to vary from 500 m to 2000 m so likely need to use large lai areal
# average, maybe bigger than the snowsurvey measurements currently used.. could
# do lidar.

# The final radiation dataset contains:
# 1. measured shortwave in from the CNR4 at Fortress Ridge South (aka or FFR crhm QSi choice)
# 2. shortwave out is from measured SW in above modelled albedo for the snow and steadty for canopy and a weighed average depending on the sky view fraction to calculate the final value
# 3. lw in is calculated using sicart 2006 eq. 9, LW irradiance in open environments
# 4. lw out is calculated from the snow and tree IRTC and a sky view fraction based weighted average 
# 5. net rad is the balance of above (SWin - SWout + LWin - LWout)

# read data ----

ec_in <- readRDS('data/low-tower/low_tower_15min_2021_2023_qc_rough_semifill.rds')

mod_in <- CRHMr::readOutputFile('../crhm-analysis/output/2023-11-14-13-13-39_fortress_forest_ridge_ac_output.txt', timezone = 'Etc/GMT+6')

mod_snow_surface_temp <- mod_in |> 
  select(datetime, hru_tsf.1) |> 
  pivot_longer(!datetime)

ffr_irtc <- readRDS('../met-data-processing/data/ffr_met_main_qaqc.rds')

# the nr lite measures the balance of sw/lw in/out 
ffr_nrlite <- readRDS('../met-data-processing/data/waterloo_1000_met_main.rds') |> 
  select(datetime, 
         meas_net_rad_15m = NR_Wm2_Avg, # TOP ~ 15m
         meas_net_rad_2m = NR_Wm2_2_Avg, # BOTTOM ~ 2m
         )

ffr_met <-
  readRDS('../met-data-processing/data/ffr_crhm_obs_qaqc_gap_fill.rds') 

# The CNR4 senor measures temperature (K deg), the equations for conversion to W m2 are:
#   'Correct the long-wave radiation values from pyrgeometers
#     long_in_corr = long_in+5.67e-8*cnr4_T_K^4
#     long_out_corr = long_out+5.67e-8*cnr4_T_K^4

frg_met <- readRDS('../met-data-processing/data/frg_met_server.rds') |> 
  select(datetime, 
         sw_in = short_in_Avg,
         sw_out = short_out_Avg,
         lw_in = long_in_corr_Avg,
         lw_out = long_out_corr_Avg)

pwl_met <- readRDS('../met-data-processing/data/pwl_met_server.rds') |> 
  select(datetime, 
         sw_in = short_in_Avg,
         sw_out = short_out_Avg,
         lw_in = long_in_corr_Avg,
         lw_out = long_out_corr_Avg,
         net_short_Avg,
         net_long_Avg,
         net_rad_Avg)

# pwl_met |> 
#   pivot_longer(!datetime) |> 
#   ggplot(aes(datetime, value, colour = name)) + geom_line()
# 
# ggplotly()

# constants ----

lat <- 50.82579670
lon <- -115.19682103
SM <- radiation::standard_meridian(ffr_met$datetime)
LAI <- 1.846667 # avg of snow survey pts surrounding the EC tower taken as hemi photos on the ground surface
cc <- 0.5381589 # avg of snow survey pts surrounding the EC tower taken as hemi photos on the ground surface
sigma <- 5.67e-8 # W m ^-2^ K^-4^
emissivity_canopy <- 0.98 # emissivity of canopy
emissivity_snow <- 0.99 # emissivity of snow (used in CRHM)
B <- 0.023 # shortwave extinction, empirically derived for a Colorado Pine Forest [@Pomeroy2009]
Vf <- 1 - cc # this is not what CRHM uses but found that the eqn below was way off for our site (0.28 vs. 0.5)
# Vf <- 0.45-0.29 * log(LAI) # Pomeroy 2002 
albedo_c <- (0.08+0.18)/2 # [@Pomeroy1996; Betts1997]
albedo_s <- (0.8+0.9)/2 # [@Melloh2002]
Gsc_watt <- 1367 # W/m2

vf_tree_snow <- data.frame(name = c('IRTC_midTree', 'IRTC_snow'),
                  vf = c(1-Vf, Vf))

steph_boltzman <- function(emissivity, temperature){
  watts <- emissivity * sigma * (temperature + 273.15)^4
  return(watts)
}

sw_extra <- function(datetime){
  d_r <- radiation::inverse_distance(datetime)
  sw_extra <- Gsc_watt * d_r # without taking into account angle of incidence
  return(sw_extra)
}

# lw above canopy @sicart2006, eq. 9

lw_in_cloudy <- function(vapour_pressure, rh, air_temp_celc, sw_in, sw_extra){
  tau_atm <- sw_in/sw_extra
  T_K <- air_temp_celc + 273.15
  lw_in <- 1.24*(vapour_pressure/T_K)^{1/7}*(1 + 0.44 * rh/100 - 0.18 * tau_atm) * sigma * T_K^4
}

emissivity_air <- function(air_temp, vapour_pressure){
  return(1.24*((vapour_pressure)/(air_temp))^{1/7})
}

canopy_transmitance <- function(LAI, solar_angle, extinction_efficiency){ # from pomeroy 1996
  tau <- exp(-(extinction_efficiency*LAI)/(sin(solar_angle)))
  return(tau)
}

# calculate incoming longwave radiation to open (aka above canopy) as in sicart 2006 ----

lw_calcs <- ffr_met |>
  mutate(
    air_temp_kelvin = t + 273.15,
    es = psychRomet::tetens(t),
    ea = psychRomet::actual_vapour_pressure(es, rh / 100) * 10,
    # times 10 for kpa to mb which is what the emissivity fn needs
    emissivity_air = emissivity_air(air_temp_kelvin, ea),
    sw_extra = sw_extra(datetime),
    lw_in_no_clouds = steph_boltzman(emissivity_air, t),
    lw_in_canopy_top = lw_in_cloudy(
      vapour_pressure = ea,
      rh = rh,
      air_temp_celc = t,
      sw_in = Qsi,
      sw_extra = sw_extra
    )
  )

lw_calcs_long <- lw_calcs |> 
  select(datetime, lw_in_no_clouds, lw_in_canopy_top) |> 
  pivot_longer(starts_with('lw_in')) 

lw_calcs_long |> 
  ggplot(aes(datetime, value, colour = name)) + geom_line()

meas_lw <- frg_met |> 
  select(datetime, lw_in) |> pivot_longer(!datetime)
  
# # using the corrected measured LW looks good 

rbind(lw_calcs_long, meas_lw) |>
ggplot(aes(datetime, value, colour = name)) +
geom_line()

# ggplotly()
 
# calculate outgoing longwave radiation using snow surf temp and canopy temp ---- 
 
ffr_irtc_temp <- ffr_irtc |> 
   select(datetime, IRTC_snow, IRTC_midTree) |> 
   pivot_longer(!datetime) |> 
   left_join(vf_tree_snow) |> 
   group_by(datetime) |> 
   mutate(IRTC_w_avg = weighted.mean(value, vf)) |> 
   select(-vf) |> 
   pivot_wider()
 
# ffr_met |> 
#   left_join(ffr_irtc_temp) |> 
#    pivot_longer(c(t, IRTC_snow, IRTC_midTree, IRTC_w_avg)) |> 
#    select(datetime, name, value) |> 
#    rbind(mod_snow_surface_temp) |> 
#    ggplot(aes(datetime, value, colour = name)) + geom_line()
#  
# ggplotly()

vf_tree_snow <- data.frame(name = c('IRTC_midTree_watts', 'IRTC_snow_watts'),
                           vf = c(1-Vf, Vf))

ffr_irtc_watts <- ffr_irtc |> 
  select(datetime, IRTC_snow, IRTC_midTree) |> 
  mutate(IRTC_snow_watts = steph_boltzman(emissivity_snow, IRTC_snow),
         IRTC_midTree_watts = steph_boltzman(emissivity_canopy, IRTC_midTree)) |> 
  pivot_longer(c(IRTC_snow_watts, IRTC_midTree_watts)) |> 
  left_join(vf_tree_snow) |> 
  group_by(datetime) |> 
  mutate(IRTC_w_avg_watts = weighted.mean(value, vf)) |> 
  select(-vf) |> 
  pivot_wider() |> 
  select(datetime, IRTC_snow_watts, IRTC_midTree_watts, IRTC_w_avg_watts) 

meas_lw <- frg_met |> 
  select(datetime, lw_out) |> pivot_longer(!datetime)

ffr_irtc_watts |> pivot_longer(!datetime) |>
  rbind(meas_lw) |> 
  ggplot(aes(datetime, value, colour = name)) +geom_line()

# ggplotly()

# calculate shortwave out using CRHM output albedo from the albedo_Richard module ----

# calculate shortwave in to the snowpack ----

# part of this are from CE836 final assignment and the algebra for SW to the snowpack is from PhD proposal

sw_final <- ffr_met |> 
  left_join(mod_in) |> 
  select(datetime, Qsi, albedo_snow_crhm = Albedo.1) |> 
  mutate( sim_st = radiation::solar_time(datetime, lon, SM),
          sim_sha = radiation::solar_hour_angle(solar_time = sim_st),
          AOI_rad = radiation::angle_of_incidence_horizontal(datetime = datetime, 
                                                             latitude_deg = lat, 
                                                             solar_hour_angle = sim_sha),
          AOI_hor_deg = acos(AOI_rad) / (pi /180),
          Q_ext = 1.081*AOI_rad*cos(AOI_rad),
          tau = canopy_transmitance(LAI, AOI_rad, Q_ext),
          # SW_net = Qsi * (1-albedo_c - tau*(1-albedo_snow_crhm)), # from pomeroy2009 used to check below
          SW_in = Qsi + (Qsi*tau*albedo_snow_crhm), # below SW_out is from phd proposal and add up to same as pom 2009
          SW_out_pom_09 = (Qsi * tau) + (Qsi * albedo_c),
          SW_out = (Qsi * tau * (1-albedo_snow_crhm)) + (Qsi * albedo_c * cc) + (Qsi * albedo_snow_crhm * Vf), # new update from ac 2023-11-16 as slightly diff from proposal canopy example, this almost doubles the SW out compared to one above this
          SW_net_ac = SW_in - SW_out) |> 
  select(datetime, sw_in = SW_in, sw_out = SW_out)

# compile file df of our radiation measurements ----

lw_final <- frg_met |> 
  select(datetime, lw_in) |> # switched from modelled to measured incoming longwave after figuring out the right LW column to use
  left_join(ffr_irtc_watts |> select(datetime, lw_out = IRTC_w_avg_watts))

rad_out <- left_join(sw_final, lw_final) |> 
  mutate(net_rad = sw_in - sw_out + lw_in - lw_out)

saveRDS(rad_out, 'data/energy-balance/above_canopy_energy_balance_15min.rds')

# not useful comparing modeled snowpack net radiation here as we are looking at an air parcel above the canopy
rad_out |>
  left_join(ffr_nrlite ) |>
  pivot_longer(!datetime) |>
  ggplot(aes(datetime, value, colour = name)) +geom_line()

# ggplotly()

rad_out |>
  left_join(ffr_nrlite)  |> 
  ggplot(aes(meas_net_rad_15m, net_rad)) +
  geom_point(alpha = 0.5) +
  ylab('Measured and Modelled Radiation Fluxes (W m-2)') +
  xlab('NRLITE (W m-2)') +
  geom_abline()

ggsave('figs/energy-bal-assessment/mod_meas_net_rad_vs_nrlite_above_canopy.png', width = 6, heigh = 6)

rad_out_daily <- rad_out |> 
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

saveRDS(rad_out_daily, 'data/energy-balance/above_canopy_energy_balance_daily.rds')

nrlite_out_daily <- ffr_nrlite |> 
  select(datetime, nrlite_net_rad = meas_net_rad_15m)|> 
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

saveRDS(nrlite_out_daily, 'data/energy-balance/above_canopy_nrlite_daily.rds')
