# this script was created for the INARCH presentation on unloading we wanted a
# canopy top shear stress which should indicate the force acting on the canopy
# however the waterloo EC system records at 30 min so we attempt here to estimate
# shear stress from wind speed by building a relationship with mid canopy wind
# and sub-canopy shear stress

library(tidyverse)
library(CRHMr)
theme_set(theme_bw())

to_long <- function(from, to, class, quality, notes, storm_id){
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime, class, quality, notes, storm_id)
  
  return(out)
}


canopy_snow_events <- read.csv('../unloading/data/snow_in_canopy_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
         storm_id = as.Date(from, tz = 'Etc/GMT+6')) |> filter(quality < 3)

canopy_snow_long <- purrr::pmap_dfr(canopy_snow_events, to_long)

canopy_snow_long$storm_id <- as.Date(canopy_snow_long$storm_id)

low_tower <- readRDS('data/low-tower/low_tower_15min_2021_2023_qc_rough_semifill.rds') |> 
  select(datetime, tau_low = tau)

low_tower <- low_tower |> 
  left_join(canopy_snow_long) |> 
  filter(is.na(storm_id) == F) 

high_tower_30 <- readRDS('data/high-tower/ec_high_tower_30_min_2021_2023_qc_rough_semifill.rds') |> 
  select(datetime, tau_high = Tau)

high_tower_30 <- high_tower_30 |> 
  left_join(canopy_snow_long) |> 
  filter(is.na(storm_id) == F)

mid_tower_wind <- readRDS('../met-data-processing/data/ffr_crhm_obs_qaqc_gap_fill.rds') |> 
  select(datetime, mid_canopy_wind = u)

mid_tower_wind_30 <- mid_tower_wind |> 
  mutate(datetime = lubridate::floor_date(datetime, unit = '30 mins')) |> 
  group_by(datetime) |> 
  summarise(mid_canopy_wind = mean(mid_canopy_wind))

mid_tower_wind_30 <- mid_tower_wind_30 |> 
  left_join(canopy_snow_long) |> 
  filter(is.na(storm_id) == F)

mid_tower_wind$mid_canopy_wind_squared <- mid_tower_wind$mid_canopy_wind^2
mid_tower_wind_30$mid_canopy_wind_squared <- mid_tower_wind_30$mid_canopy_wind^2


wind_tau <- inner_join(mid_tower_wind, low_tower) |> 
  inner_join(high_tower_30)

# going to 30 min avgs doesnt make a difference
wind_tau_30 <- inner_join(mid_tower_wind_30, high_tower_30)

# plot mid canopy wind with 3 m shear stress RAW ----

lm_low <- lm(tau_low ~ 0 + mid_canopy_wind, data = wind_tau)
lm_rsq <- summary(lm_low)$r.squared |> round(2)

ggplot(wind_tau, aes(mid_canopy_wind, tau_low)) + 
  geom_point()  +
  # geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x) +
  geom_abline(intercept = 0, slope = lm_low$coefficients[1], colour = 'red') +
  labs(x = expression(Mid~Canopy~Wind~Speed ~ "("*m~s^-1*")"),
       y = expression(Shear~Stress ~ "("*N ~m^-2*")")) +
  annotate("label", x = 1, y = 0.7,
           label = paste0("italic(R) ^ 2  ==", lm_rsq), parse = TRUE) + 
  ggtitle('Below Canopy Shear Stress (3 m)')

ggsave(
  'figs/estimate_tau_from_wind/mid_cnpy_wind_VS_canopy_low_shear_stress.png',
  device = png,
  width = 5,
  height = 4
)


# plot mid canopy wind with 3 m shear stress TRANSFORMED ----
# according to JP tau is proportional to the square of the wind speed

lm_low_sq <- lm(tau_low ~ 0 + mid_canopy_wind_squared, data = wind_tau)
lm_rsq <- summary(lm_low_sq)$r.squared |> round(2)

ggplot(wind_tau, aes(mid_canopy_wind_squared, tau_low)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = lm_low_sq$coefficients[1], colour = 'red') +
  labs(x = expression(Mid~Canopy~Wind~Speed  ~ SQUARED~ "("*m~s^-1*")"),
       y = expression(Shear~Stress~ "("*N ~m^-2*")")) +
  annotate("label", x = 3, y = 0.7,
           label = paste0("italic(R) ^ 2  ==", lm_rsq), parse = TRUE)+ 
  ggtitle('Below Canopy Shear Stress (3 m)')

ggsave(
  'figs/estimate_tau_from_wind/mid_cnpy_wind_squared_VS_canopy_low_shear_stress.png',
  device = png,
  width = 5,
  height = 4
)

# ggplot(wind_tau, aes(mid_canopy_wind, tau_high)) + 
#   geom_point()+
#   geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x) +
#   xlab('Mid Canopy Wind Speed (m/s)') +
#   ylab('15 m Shear Stress (N m-2)') 
# 
# ggplot(wind_tau, aes(mid_canopy_wind^2, tau_high)) + 
#   geom_point()+
#   geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x) +
#   xlab('Mid Canopy Wind Speed (m/s)') +
#   ylab('15 m Shear Stress (N m-2)') 

# plot mid canopy wind with canopy top 15 m shear stress RAW ----

lm_high <- lm(tau_high ~ 0 + mid_canopy_wind, data = wind_tau)
lm_rsq <- summary(lm_high)$r.squared |> round(2)

ggplot(wind_tau, aes(mid_canopy_wind, tau_high)) + 
  geom_point()  +
  # geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x) +
  geom_abline(intercept = 0, slope = lm_high$coefficients[1], colour = 'red')  +
  labs(x = expression(Mid~Canopy~Wind~Speed ~ "("*m~s^-1*")"),
       y = expression(Shear~Stress ~ "("*N ~m^-2*")")) +
  annotate("label", x = 1, y = 1.2,
           label = paste0("italic(R) ^ 2  ==", lm_rsq), parse = TRUE)+ 
  ggtitle('Canopy Top Shear Stress (15 m)')

ggsave(
  'figs/estimate_tau_from_wind/mid_cnpy_wind_VS_canopy_top_shear_stress.png',
  device = png,
  width = 5,
  height = 4
)

# plot mid canopy wind with canopy top 15 m shear stress TRANSFORMED ----
# according to JP tau is proportional to the square of the wind speed

lm_high_sq <- lm(tau_high ~ 0 + mid_canopy_wind_squared, data = wind_tau)
lm_rsq <- summary(lm_high_sq)$r.squared |> round(2)

ggplot(wind_tau, aes(mid_canopy_wind_squared, tau_high)) + 
  geom_point()  +
  # geom_smooth(method = 'lm', se = F, formula = y ~ 0 + x) +
  geom_abline(intercept = 0, slope = lm_high_sq$coefficients[1], colour = 'red')  +
  labs(x = expression(Mid~Canopy~Wind~Speed~SQUARED~ "("*m~s^-1*")"),
       y = expression(Shear~Stress ~ "("*N ~m^-2*")")) +
  annotate("label", x = 3, y = 1.25,
           label = paste0("italic(R) ^ 2  ==", lm_rsq), parse = TRUE)+ 
  ggtitle('Canopy Top Shear Stress (15 m)')

ggsave(
  'figs/estimate_tau_from_wind/mid_cnpy_wind_squared_VS_canopy_top_shear_stress.png',
  device = png,
  width = 5,
  height = 4
)

#----
### John suggests: I suggest you use the square of U versus Tau relationship
### from the sub-canopy EC system.  It had a decent r^2 and the turbulence there
### seemed more realistic for the mid-canopy.  The top of canopy seemed really
### different.
#----

wind_tau_sel <- wind_tau |> 
  select(datetime, mid_canopy_wind_squared, tau_low)

mid_u_col_sqrd <- 1
tau_low_col <- 2

lm_mid_wnd_sq_low_tau <- regress(
  wind_tau_sel,
  primary.columns = tau_low_col,
  wind_tau_sel,
  forceOrigin = T,
  secondary.columns = mid_u_col_sqrd,
  plot = F,
  quiet = F
) 

saveRDS(lm_mid_wnd_sq_low_tau, 'data/est_tau_from_wnd/lm_mid_wnd_sqrd_low_tau.rds')
