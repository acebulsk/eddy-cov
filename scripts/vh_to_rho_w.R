# script to handle non-linear adjustment to hygrometer output

library(wxlogR)

raw_path <- 'data/hi-freq/raw/subset-15min-ascii/'
out_path <- 'data/hi-freq/clean/subset-15min-ascii-calcrho/'

l <- list.files('data/hi-freq/raw/subset-15min-ascii', full.names = F)

# constants from logger program

V_0 <- 3259 # Input Voltage at KH20 (according to table A-2 in KH20-manual for dry conditions) [mV] Should update to value on calibration sheet. 
KW_high <- 0.181 # sensor specific S/N 1527, scaled, dry vapour range [m^3 / (g cm)]
x <- 1  # Path length of KH20 (cm)
xkw <- x * XKW_high  # Path Length * Water Vapor absorption coefficient (m^3 / g)

# compare to lindsay nightmare email chain

(log(870.5355) - log(V_0)) / -0.185
(log(490.8061) - log(V_0)) / -0.185

# look at one file to check calculations 

# df <- wxlogR::load_CS_1000(paste0(raw_path, l[1]), timezone = 'GMT-6')
# 
# df$ln_mV <- log(df$vh) # check KH20 manual fig. A-1 to ensure log(mV) aka log(vh) matches the vapor density (g.m3)
# 
# df$rho_w <- (log(df$vh) - log(V_0)) / xkw # g / m3, equation is A-3 from KH20 manual

# check calculated water vapour density against absolute humidity, not sure if this is appropriate comparison as does not match up well for Feb 10, 2022 - maybe instrument error. 

# met <- readRDS('../../analysis/interception/data/met/met_main_th.rds') |> 
#   select(TIMESTAMP = datetime, AirTC_towerTop, RH_towerTop) |> 
#   mutate(TIMESTAMP = round(TIMESTAMP, "mins"))
# 
# ec <- df |> mutate(TIMESTAMP = round(TIMESTAMP, "mins"))
# 
# ec_met <- left_join(ec, met)
# 
# ec_met$rho_air <- psychRomet::absolute_humidity(ec_met$AirTC_towerTop, ec_met$RH_towerTop/100) * 1000


calc_rho_w <- function(file) {
  df <- wxlogR::load_CS_1000(paste0(raw_path, file), timezone = 'GMT-6')
  
  df$rho_w <- (log(df$vh) - log(V_0)) / -xkw # g / m3, equation is A-3 from KH20 manual
  
  df <- df[c('Ux', 'Uy', 'Uz', 'Ts', 'rho_w')]
  
  write.csv(df, paste0(out_path, file), row.names = F)
}

lapply(l, calc_rho_w)

df_out <- read.csv(paste0(out_path, l[1]))
