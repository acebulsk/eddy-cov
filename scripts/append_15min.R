# combine all 15 min EC downloads, this script uses the output from the campbell sci card 
# convert program which extracts the compressed TOA3 files to TOA5 which we load in here
# hi freq data was moved to hard drive (compressed data on hard drive and one drive)
library(dplyr)
library(wxlogR)

# get just the 15 min data
files <- list.files('../../field-downloads/met-data/hi-freq/clean', pattern = '*flux15.dat', full.names = T)

# loop through files and bind together
bind <- map_dfr(files, wxlogR::load_CS_1000, skip_4_dat = 4, timezone = 'GMT-6') |> distinct()

# check data gaps against complete time series range
date_range <- range(bind$TIMESTAMP)
date_range

date_seq <- as.POSIXct(seq(date_range[1], date_range[2], by = 15 * 60))

missing_records <- length(date_seq) - nrow(bind)

print(paste('The number of records missing is: ', missing_records))

# join on full sequence
date_seq_df <- data.frame(TIMESTAMP = date_seq)

df_cont <- date_seq_df |> 
  left_join(bind, by = 'TIMESTAMP')

write.csv(df_cont, 'data/eddy_cov_15min_combined.csv')
saveRDS(df_cont, 'data/eddy_cov_15min_combined.rds')
