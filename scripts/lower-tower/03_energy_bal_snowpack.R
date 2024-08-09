# Energy Balance for the snowpack below the 3 m EC system for quality assessment
# of EC measurements

library(tidyverse)
library(plotly)

# The final radiation dataset contains:
# 1. shortwave in to the snowpack (below canopy) simulated tau from pomeroy 1996
# 2. shortwave out is shortwave in multiplied by the snowpack albedo
# 3. lw in is calculated using pomeroy 2009 aka air temp and a empiraclal portion for the canopy sky view fraction
# 4. lw out is calculated from the snow temperature
# 5. net rad is the balance of above (SWin - SWout + LWin - LWout)

# constants 
lat <- 50.82579670
lon <- -115.19682103
SM <- -105 # standard meridian for calagry
LAI <- 0.43 # hemi image taken on 2022-08 by ac under the low-ec tower
sigma <- 5.67e-8 # W m ^-2^ K^-4^
emissivity_canopy <- 0.98 # emissivity of canopy
emissivity_snow <- 0.99 # emissivity of snow (used in CRHM)
B <- 0.023 # shortwave extinction, empirically derived for a Colorado Pine Forest [@Pomeroy2009]
Vf <- 0.45-0.29 * log(LAI) # Pomeroy 2002 
albedo_c <- (0.08+0.18)/2 # [@Pomeroy1996; Betts1997]
albedo_s <- (0.8+0.9)/2 # [@Melloh2002]

vf_tree_snow <- data.frame(name = c('IRTC_midTree', 'IRTC_snow'),
                  vf = c(1-Vf, Vf))

steph_boltzman <- function(emissivity, temperature){
  watts <- emissivity * sigma * (temperature + 273.15)^4
  return(watts)
}

# functions from @pomeroy2009 for longwave

canopy_transmitance <- function(LAI, solar_angle, extinction_efficiency){ # from pomeroy 1996
  tau <- exp(-(extinction_efficiency*LAI)/(sin(solar_angle)))
  return(tau)
}

emissivity_air <- function(air_temp, vapour_pressure){
  return(1.24*((vapour_pressure)/(air_temp))^{1/7})
}

# this is from sicart2006
lw_in_simple <- function(air_temp, emissivity_air){
  lw_in <- (sigma*(air_temp)^4)*(Vf*emissivity_air + (1-Vf) * emissivity_canopy)
  return(lw_in)
}

# mods to sicart2006 by pomeroy2009
lw_in_bl_cpy <- function(air_temp, emissivity_air, K_star_H){
  lw_in <- (sigma*air_temp^4) * 
    (Vf*emissivity_air + (1-Vf) * emissivity_canopy) + 
    (K_star_H * B)
  return(lw_in)
}

ec_in <- readRDS('data/low-tower/low_tower_15min_2021_2023_qc_rough_semifill.rds')

mod_in <- CRHMr::readOutputFile('../crhm-analysis/output/2023-11-14-13-13-39_fortress_forest_ridge_ac_output.txt', timezone = 'Etc/GMT+6')

mod_snow_surface_temp <- mod_in |> 
  select(datetime, hru_tsf.1) |> 
  pivot_longer(!datetime)

ffr_irtc <- readRDS('../met-data-processing/data/ffr_met_main_qaqc.rds')

ffr_irtc_temp <- ffr_irtc |> 
  select(datetime, IRTC_snow, IRTC_midTree) |> 
  pivot_longer(!datetime) |> 
  left_join(vf_tree_snow) |> 
  group_by(datetime) |> 
  mutate(IRTC_w_avg = weighted.mean(value, vf)) |> 
  select(-vf) |> 
  pivot_wider()

ffr_met <-
  readRDS('../met-data-processing/data/ffr_crhm_obs_qaqc_gap_fill.rds')  |>
  left_join(ffr_irtc_temp)

frs_met <- readRDS('../met-data-processing/data/frs_met_server.rds') |> 
  select(datetime, 
         sw_in = short_in_Avg,
         sw_out = short_out_Avg,
         lw_in = long_in_Avg,
         lw_out = long_out_Avg)

pwl_met <- readRDS('../met-data-processing/data/pwl_met_server.rds') |> 
  select(datetime, 
         sw_in = short_in_Avg,
         sw_out = short_out_Avg,
         lw_in = long_in_Avg,
         lw_out = long_out_Avg,
         net_short_Avg,
         net_long_Avg,
         net_rad_Avg)

# pwl_met |> 
#   pivot_longer(!datetime) |> 
#   ggplot(aes(datetime, value, colour = name)) + geom_line()
# 
# ggplotly()

# calculate incoming longwave radiation as in pomeroy 2009 ----

lw_calcs <- ffr_met |>
  mutate(
    air_temp_kelvin = t + 273.15,
    es = psychRomet::tetens(t),
    ea = psychRomet::actual_vapour_pressure(es, rh / 100) * 10,
    # times 10 for kpa to mb which is what the emissivity fn needs
    emissivity_air = emissivity_air(air_temp_kelvin, ea),
    lw_in_simple = lw_in_simple(air_temp_kelvin, emissivity_air),
    sim_st = radiation::solar_time(datetime, lon, SM),
    sim_sha = radiation::solar_hour_angle(solar_time = sim_st),
    AOI_rad = radiation::angle_of_incidence_horizontal(datetime = datetime, 
                                                       latitude_deg = lat, 
                                                       solar_hour_angle = sim_sha),
    AOI_hor_deg = acos(AOI_rad) / (pi /180),
    Q_ext = 1.081*AOI_rad*cos(AOI_rad),
    tau = canopy_transmitance(LAI, AOI_rad, Q_ext),
    K_star_H = Qsi * (1-albedo_c - tau*(1-albedo_s)),
    lw_in_bl_cpy = lw_in_bl_cpy(air_temp_kelvin, emissivity_air, K_star_H)
  )

sim_lw <- lw_calcs |> 
  select(datetime, lw_in_simple, lw_in_bl_cpy) |> 
  pivot_longer(!datetime)

meas_lw <- frs_met |> 
  select(datetime, lw_in) |> pivot_longer(!datetime)
  
# something off with measured lw
# bareley a difference in lw_in_bl_cpy and the simple one
 rbind(sim_lw, meas_lw) |>
  ggplot(aes(datetime, value, colour = name)) +
  geom_line()
 
# calculate outgoing longwave radiation using snow surf temp and canopy temp ---- 
 
ffr_met |>
   pivot_longer(c(t, IRTC_snow, IRTC_midTree, IRTC_w_avg)) |>
   select(datetime, name, value) |>
   rbind(mod_snow_surface_temp) |>
   ggplot(aes(datetime, value, colour = name)) + geom_line()

ggplotly()

ffr_irtc_watts <- ffr_irtc |> 
  select(datetime, IRTC_snow) |> 
  mutate(IRTC_snow_watts = steph_boltzman(emissivity_snow, IRTC_snow)) |> 
  select(datetime, IRTC_snow_watts) 

# ffr_irtc_watts |> pivot_longer(!datetime) |> 
#   ggplot(aes(datetime, value, colour = name)) +geom_line()
# 
# ggplotly()

# calculate shortwave out using CRHM output albedo from the albedo_Richard module ----

# calculate shortwave in to the snowpack ----

# part of this are from CE836 final assignment and the algebra for SW to the snowpack is from PhD proposal

sw_final <- ffr_met |> 
  left_join(mod_in) |> 
  select(datetime, Qsi, albedo_snow_crhm = Albedo.1) |> 
  mutate(sim_st = radiation::solar_time(datetime, lon, SM),
          sim_sha = radiation::solar_hour_angle(solar_time = sim_st),
          AOI_rad = radiation::angle_of_incidence_horizontal(datetime = datetime, 
                                                             latitude_deg = lat, 
                                                             solar_hour_angle = sim_sha),
          AOI_hor_deg = acos(AOI_rad) / (pi /180),
          Q_ext = 1.081*AOI_rad*cos(AOI_rad),
          tau = canopy_transmitance(LAI, AOI_rad, Q_ext),
          SW_in = Qsi * tau,
          SW_out = Qsi * tau * albedo_snow_crhm) |> 
  select(datetime, sw_in = SW_in, sw_out = SW_out)

# compile file df of our radiation measurements ----

lw_final <- sim_lw |>
  pivot_wider() |> 
  select(datetime, lw_in = lw_in_bl_cpy) |> 
  left_join(ffr_irtc_watts |> select(datetime, lw_out = IRTC_snow_watts))

# not useful comparing modeled snowpack net radiation here as we are looking at an air parcel above the canopy
rad_out <- left_join(sw_final, lw_final) |> 
  mutate(net_rad = sw_in - sw_out + lw_in - lw_out) |> 
  left_join(mod_in |> select(datetime, mod_net_rad = R_n.1))

saveRDS(rad_out, 'data/energy-balance/snowpack_energy_balance_15min.rds')

rad_out_daily <- rad_out |> 
  pivot_longer(!datetime) |> 
  mutate(date = as.Date(datetime, tz = 'Etc/GMT+6')) |> 
  group_by(date, name)  |> 
  summarise(
    count_nans = sum(is.na(value)),
    value = mean(value, na.rm = T)) |> 
  mutate(value = case_when(
    count_nans > (0.1 * (24*4)) ~ NA, # check if have more than 90% of measurements
    TRUE ~ value
  ))|> 
  select(-count_nans) |>
  pivot_wider()
    
saveRDS(rad_out_daily, 'data/energy-balance/snowpack_energy_balance_daily.rds')

rad_out |> pivot_longer(!datetime) |> 
  ggplot(aes(datetime, value, colour = name)) +geom_line()
ggplotly()
