# script to handle non-linear adjustment to hygrometer output

library(wxlogR)
library(ggplot2)
library(dplyr)
library(logr)

general_log_note <- 
"For this run we added the actual calibration parameters from the CS calibration sheet for SN 1528. 
The 3m ec system has not been checked for the serial number as of yet but Lindsey said safe to assume 
since the other EC SN 1527 is in the lab."

# functions

rho_w <- function(vh, v_0, xkw) {
  rho_w <- (log(vh) - log(v_0))/-xkw # g / m3, equation is A-3 from KH20 manual
  
  return(rho_w)
}

# Calibration Values from Campbell Sci 2016-10-13 cal on SN 1528, chose to use dry scaled value as this is what was done before in the programs

cals <- read.csv('../../field-downloads/met-data/hi-freq/calibrations/2022_10_16_kh2o_1528_cal_data.csv')

v_0 <- cals$Intercept[cals$WINDOWS == 'SCALED' & cals$VAPOUR_RANGE == 'DRY 1.82-9.27 gm3'] # Input Voltage at KH20
kw <- cals$Kw[cals$WINDOWS == 'SCALED' & cals$VAPOUR_RANGE == 'DRY 1.82-9.27 gm3'] # scaled, dry vapour range [m^3 / (g cm)]
x <- cals$PATH_CM[cals$WINDOWS == 'SCALED' & cals$VAPOUR_RANGE == 'DRY 1.82-9.27 gm3'] # Path length of KH20 (cm)
xkw <- x * kw  # Path Length * Water Vapor absorption coefficient (m^3 / g)

# Create temp log file location
path <- paste0("ec-high-freq-processing_", format(Sys.time(), "%Y_%m_%d_%H%M"), ".log")
tmp <- file.path('/media/alex/CRHO PHOTOS/fortress/met-data/hi-freq/', path)

# Open log
lf <- log_open(tmp)

log_print(general_log_note, console = T)

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

date_check <- as.POSIXct(fix_string, "%Y_%m_%d_%H:%M", tz = 'GMT-6')

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

# look at one file to check calculations

df <- wxlogR::load_CS_1000(paste0(raw_path, l[7000]), timezone = 'GMT-6')

rho_w(df$vh, v_0, xkw)

# check calculated water vapour density against absolute humidity, not sure if this is appropriate comparison as does not match up well for Feb 10, 2022 - maybe instrument error.

met <- readRDS('../../analysis/interception/data/met/met_main_th.rds') |>
  select(TIMESTAMP = datetime, AirTC_towerTop, RH_towerTop) |>
  mutate(TIMESTAMP = round(TIMESTAMP, "mins"))

ec <- df |> mutate(TIMESTAMP = round(TIMESTAMP, "mins"))

ec_met <- left_join(ec, met)

ec_met$rho_air <- psychRomet::absolute_humidity(ec_met$AirTC_towerTop, ec_met$RH_towerTop/100) * 1000


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
  
  df$rho_w <- rho_w(df$vh, v_0, xkw) # g / m3, equation is A-3 from KH20 manual
  
  df <- df[c('Ux', 'Uy', 'Uz', 'Ts', 'rho_w')]
  
  write.csv(df, paste0(out_path, file_out), row.names = F)
  
  return(file_out)
}

# file_range <- 2541:8492
# file_range <- 8493:length(filename_filter)
# file_range <- 1:10

file_out_list <- paste0('FFR_low_ec_',gsub('.*highfreq_|', "", filename_filter))

parallel::mcmapply(calc_rho_w, filename_filter, file_out_list, mc.cores = 7)

# df_out <- read.csv(paste0(out_path, file_out[1]))
