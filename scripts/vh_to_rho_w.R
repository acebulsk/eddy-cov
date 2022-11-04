# script to handle non-linear adjustment to hygrometer output

library(wxlogR)
library(ggplot2)
library(dplyr)
library(logr)

# WARNING NEED TO CHECK SENSOR SERIAL NUMBER AND CALIBRATION CERT FOR PROPER VALUES STILL 

# Create temp file location
path <- paste0("ec-high-freq-processing_", format(Sys.time(), "%Y_%m_%d_%H%M"), ".log")
tmp <- file.path('data/', path)

# Open log
lf <- log_open(tmp)

# raw_path <- 'data/hi-freq/raw/subset-15min-ascii/'
# out_path <- 'data/hi-freq/clean/subset-15min-ascii-calcrho/'
raw_path <- '/media/alex/CRHO PHOTOS/fortress/met-data/hi-freq/clean-for-pre-processing/'
out_path <- '/media/alex/CRHO PHOTOS/fortress/met-data/hi-freq/calc-rho-w-for-ep/'

l <- list.files(raw_path, full.names = F)

# rm first partial file 
l_sub <- l[2:length(l)]

# querey the date portion of the string
string_check <- gsub('.*highfreq_|.dat.*', "", l_sub)

# strptime cannot handle hour mins with no separator 
fix_string <- paste0(substr(string_check, 1,13), ':', substr(string_check, 14,15))

date_check <- as.POSIXct(strptime(fix_string, "%Y_%m_%d_%H:%M"), tz = 'GMT-6')

# now we need to reduce the raw files list too rm duplicates
df_check <- data.frame(datetime = date_check, filename = l_sub, val = 1)

df_check_reduce <- df_check[order(df_check[,'datetime']),]
df_check_reduce <- df_check_reduce[!duplicated(df_check_reduce$datetime),]

# check data gaps

wxlogR::plot_data_gaps(df_check_reduce$datetime)

full_seq <- wxlogR::datetime_seq_full(df_check_reduce$datetime)
df_full <- data.frame(datetime = full_seq)

df_compare <- dplyr::left_join(df_full, df_check_reduce)

filename_filter <- df_compare$filename[is.na(df_compare$filename) == F]

# constants from logger program

V_0 <- 3259 # Input Voltage at KH20 (according to table A-2 in KH20-manual for dry conditions) [mV] Should update to value on calibration sheet. 
KW_high <- 0.181 # sensor specific S/N 1527, scaled, dry vapour range [m^3 / (g cm)]
x <- 1  # Path length of KH20 (cm)
xkw <- x * KW_high  # Path Length * Water Vapor absorption coefficient (m^3 / g)

# compare to Lindsey nightmare email chain

# (log(870.5355) - log(V_0)) / -0.185
# (log(490.8061) - log(V_0)) / -0.185
# 
# # look at one file to check calculations 
# 
# df <- wxlogR::load_CS_1000(paste0(raw_path, l[7000]), timezone = 'GMT-6')
# 
# df$ln_mV <- log(df$vh) # check KH20 manual fig. A-1 to ensure log(mV) aka log(vh) matches the vapor density (g.m3)
# 
# df$rho_w <- (log(df$vh) - log(V_0)) / -xkw # g / m3, equation is A-3 from KH20 manual
# 
# # check calculated water vapour density against absolute humidity, not sure if this is appropriate comparison as does not match up well for Feb 10, 2022 - maybe instrument error. 
# 
# met <- readRDS('../../analysis/interception/data/met/met_main_th.rds') |>
#   select(TIMESTAMP = datetime, AirTC_towerTop, RH_towerTop) |>
#   mutate(TIMESTAMP = round(TIMESTAMP, "mins"))
# 
# ec <- df |> mutate(TIMESTAMP = round(TIMESTAMP, "mins"))
# 
# ec_met <- left_join(ec, met)
# 
# ec_met$rho_air <- psychRomet::absolute_humidity(ec_met$AirTC_towerTop, ec_met$RH_towerTop/100) * 1000


calc_rho_w <- function(file_in, file_out) {
  log_start <- paste("Started file: ", 
                     raw_path,
                     file_in,
                     '\n',
                     "Writing file to: ", 
                     out_path,
                     file_out)
  
  # Send message to log
  log_print(log_start, console = T)
  
  df <- wxlogR::load_CS_1000(paste0(raw_path, file_in), timezone = 'GMT-6')
  
  df$rho_w <- (log(df$vh) - log(V_0)) / -xkw # g / m3, equation is A-3 from KH20 manual
  
  df <- df[c('Ux', 'Uy', 'Uz', 'Ts', 'rho_w')]
  
  write.csv(df, paste0(out_path, file_out), row.names = F)
  
  return(file_out)
}

# file_range <- 2541:8492
# file_range <- 8493:length(filename_filter)
# file_range <- 1:10

file_out_list <- paste0('FFR_low_ec_',gsub('.*highfreq_|', "", filename_filter))


parallel::mcmapply(calc_rho_w, filename_filter, file_out_list, mc.cores = 8)

df_out <- read.csv(paste0(out_path, file_out[1]))
