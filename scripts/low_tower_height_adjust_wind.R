# need to adjust wind speed due to height change on Feb 17, 2022
# Tower raised at 5 pm Feb 17 (MST) aka 6 pm CDT / MDT
# 
# Before Move:
#   - bottom of EC (top of bar) - 215 cm
# - top of arm - 255.5 cm
# - mid point of claws - 230 cm
# 
# After raise:
#   - mid point of claws - 295 cm
# - H20 centre - 295 cm
# 
# Compass Direction is relative to the CSAT block and CSAT transducer which should point North if ANGLE_FROM_NORTH == 0
# i.e S CSAT block -- > Transducer N
# compass offset is negative when West of North, and positive when East of North
# wind_dir is raw direction 
# wind_dir_compass has offset applied (set as -116 from last install as of jun 1 2022) 

# TODO need to bring in wind profile parameters and adjust low_ec wind from 2 m to 3 m

library(tidyverse)
library(weatherdash)

# need to measure this in the field still (2022-06-01)
temp_offset <- 125

# load ####


low_wind <- readRDS('data/eddy_cov_15min_combined.rds') |> 
  select(TIMESTAMP, low_ec_rslt_wnd_spd = result_wind_spd, wind_dir, wnd_dir_compass)

# qaqc ####


low_wind_qc <- low_wind |> 
  mutate(    
    mean_ws = mean(low_ec_rslt_wnd_spd, na.rm = T),
    sd_ws = sd(low_ec_rslt_wnd_spd, na.rm = T),
    stdep = (low_ec_rslt_wnd_spd - mean_ws) / sd_ws,
    flag = ifelse(stdep > 6, T, F))

ggplot(low_wind_qc, aes(TIMESTAMP, low_ec_rslt_wnd_spd, colour = flag)) + geom_point()
plotly::ggplotly()

low_wind_qc <- low_wind_qc |> filter(flag == F)

# combine with other sensors

mid_hi_wind <- readRDS('../../analysis/treefort-main/data/met/met_main.rds') |> select(TIMESTAMP, USWindSpeed_S_WVT, top_ec_rlst_wnd_spd, USWindDirection_Avg)

wnd <- left_join(low_wind, mid_hi_wind, by = 'TIMESTAMP')  |> 
  pivot_longer(c(low_ec_rslt_wnd_spd, USWindSpeed_S_WVT, top_ec_rlst_wnd_spd)) |> 
  group_by(name) |> 
  mutate(
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    stdep = (value - mean) / sd,
    flag = ifelse(stdep > 6, T, F)
  ) 

ggplot(wnd, aes(TIMESTAMP, value, colour = flag)) + 
  geom_point() + 
  facet_wrap(~name)


  mutate(
    low_ec_height = ifelse(
      TIMESTAMP >= as.POSIXct('2022-02-17 18:00:00', tz = 'CST'),
      '3m',
      '2m'
    ),
    wnd_dir_compass = (wind_dir + temp_offset) %% 365
  )

weatherdash::wind_rose(wnd, 'TIMESTAMP', 'low_ec_rslt_wnd_spd', 'wnd_dir_compass', spd_unit = 'm/s')

ggplot(wnd) +
  aes(x = top_ec_rlst_wnd_spd, y = low_ec_rslt_wnd_spd, colour = low_ec_height) +
  geom_point() +
  ylim(c(0,5))
