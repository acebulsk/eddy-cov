# script to handle non-linear adjustment to hygrometer output

library(wxlogR)
library(ggplot2)
library(dplyr)
library(logr)

# MAJOR data gaps of note 2023-01-05 to 2023-01-17 light solid on arrival on the
# 17th due to card read problem. fixed this issue in the future by ensureing
# light returns to blinking after insert and no warnings and record # increasing
# on logger screen under data menu
# 2023-06-02 2023-06-29 card problem again 

# So far have run the full 2 water years of EC data from Oct 2021 to Aug 2023
# and then ran eddy pro on the cmd line to post process these data using the aug
# 2023 measured instrument heights and using the 2016 instrument calibration.

# define some paths ----

# path_hdd <- "/media/alex/CRHO PHOTOS/field-downloads/met-data/hi-freq/"
path_hdd <- "/media/alex/phd-data/field-downloads/met-data/hi-freq/"
# raw_path <- 'data/hi-freq/raw/subset-15min-ascii/'
# out_path <- 'data/hi-freq/clean/subset-15min-ascii-calcrho/'
raw_path <- paste0(path_hdd, 'clean-for-pre-processing/')
out_path <- paste0(path_hdd, 'calc-rho-w-for-ep/')

# compile the calibration values ----

# Calibration Values from Campbell Sci 2016-10-13 cal on SN 1528, chose to use
# dry scaled value as this is what was done before in the programs

cals <- read.csv('../../field-downloads/met-data/hi-freq/calibrations/2022_10_16_kh2o_1528_cal_data.csv')

v_0 <- cals$Intercept[cals$WINDOWS == 'SCALED' & cals$VAPOUR_RANGE == 'DRY 1.82-9.27 gm3'] # Input Voltage at KH20
kw <- cals$Kw[cals$WINDOWS == 'SCALED' & cals$VAPOUR_RANGE == 'DRY 1.82-9.27 gm3'] # scaled, dry vapour range [m^3 / (g cm)]
x <- cals$PATH_CM[cals$WINDOWS == 'SCALED' & cals$VAPOUR_RANGE == 'DRY 1.82-9.27 gm3'] # Path length of KH20 (cm)
xkw <- x * kw  # Path Length * Water Vapor absorption coefficient (m^3 / g)

# params ####

file_size_filter <- 1e6 # we do not want to keep files on edge of downloads which may be incomplete
datetime_last_file_computed <- as.POSIXct('2023-03-24 10:45:00', tz = 'Etc/GMT+6') # modify the date here to match the date output below in the var 'most_recent_file'

# functions ####

rho_w <- function(vh, v_0, xkw) {
  rho_w <- (log(vh) - log(v_0))/-xkw # g / m3, equation is A-3 from KH20 manual
  
  return(rho_w)
}

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
  
  df <- wxlogR::load_CS_1000(paste0(raw_path, file_in), timezone = 'Etc/GMT+6')
  
  df$rho_w <- rho_w(df$vh, v_0, xkw) # g / m3, equation is A-3 from KH20 manual
  
  df <- df[c('Ux', 'Uy', 'Uz', 'Ts', 'rho_w')]
  
  write.csv(df, paste0(out_path, file_out), row.names = F)
  
  return(file_out)
}

# check completed files  ----

# these are the files output from this script that are already completed 

complete_files <- list.files(out_path, full.names = F) 

# we want to start the timestep after the last entry here
most_recent_file <- tail(complete_files, n =1)

paste('The last file processed was: ', most_recent_file)

# querey the date portion of the string
complete_files_date <- gsub('.*FFR_low_ec_|.dat.*', "", complete_files)

# strptime cannot handle hour mins with no separator 
complete_files_date <- paste0(substr(complete_files_date, 1,13), ':', substr(complete_files_date, 14,15))

complete_files_date <- as.POSIXct(complete_files_date, "%Y_%m_%d_%H:%M", tz = 'Etc/GMT+6')

# visualize what files we have completed so far 
df_complete_files_date <- data.frame(datetime = complete_files_date, filename = complete_files, val = 1)

wxlogR::plot_data_gaps(df_complete_files_date$datetime)
plotly::ggplotly()

# check the raw files ---- 

l <- list.files(raw_path, full.names = F)
lsz <- sapply(list.files(raw_path, full.names = T), file.size)

# querey the date portion of the string
string_check <- gsub('.*highfreq_|.dat.*', "", l)

# strptime cannot handle hour mins with no separator 
fix_string <- paste0(substr(string_check, 1,13), ':', substr(string_check, 14,15))

date_check <- as.POSIXct(fix_string, "%Y_%m_%d_%H:%M", tz = 'Etc/GMT+6')

# now we need to reduce the raw files list too rm duplicates
df_check <- data.frame(datetime = date_check, filename = l, size = lsz, val = 1)

df_check_reduce <- df_check[order(df_check[,'datetime']),]
df_check_reduce <- df_check_reduce[!duplicated(df_check_reduce$datetime),]
df_check_reduce <- df_check_reduce[df_check_reduce$size > file_size_filter,]
df_check_reduce <- df_check_reduce[df_check_reduce$datetime>datetime_last_file_computed,]

# check data gaps

ggplot(df_check_reduce, aes(x = datetime, y = size)) + geom_line()

full_seq <- wxlogR::datetime_seq_full(df_check_reduce$datetime)

df_full <- data.frame(datetime = full_seq)

df_compare <- dplyr::left_join(df_full, df_check_reduce)

filename_filter <- df_compare$filename[is.na(df_compare$filename) == F]

file_out_list <- paste0('FFR_low_ec_',gsub('.*highfreq_|', "", filename_filter))

stopifnot(length(filename_filter) == length(file_out_list))

# look at one file to check calculations

example_file_raw <- wxlogR::load_CS_1000(paste0(raw_path, filename_filter[5]), timezone = 'Etc/GMT+6')
# 
# rho_w(example_file_raw$vh, v_0, xkw)

# write out a log file #### 

general_log_note <- paste("Last run went up until", 
                          most_recent_file, ". Now we are setting up to run from", 
                          head(filename_filter, n=1),
                          "to",
                          tail(filename_filter, n=1)
                          )

path <- paste0("ec-high-freq-processing_", format(Sys.time(), "%Y_%m_%d_%H%M"), ".log")
tmp <- file.path(paste0(path_hdd, path))

# Open log
lf <- log_open(tmp)

log_print(general_log_note, console = T)

# run on full list

parallel::mcmapply(calc_rho_w, filename_filter, file_out_list, mc.cores = 7)

# df_out <- read.csv(paste0(out_path, file_out[1]))
