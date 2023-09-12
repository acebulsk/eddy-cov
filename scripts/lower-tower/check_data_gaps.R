# script to handle non-linear adjustment to hygrometer output

library(wxlogR)
library(ggplot2)
library(dplyr)


# raw_path <- 'data/hi-freq/raw/subset-15min-ascii/'
# out_path <- 'data/hi-freq/clean/subset-15min-ascii-calcrho/'
raw_path <- '/media/alex/CRHO PHOTOS/fortress/met-data/hi-freq/clean-for-ep/'

l <- list.files(raw_path, full.names = F)

# rm first partial file 
l_sub <- l[2:length(l)]

string_check <- gsub('.*highfreq_|.dat.*', "", l_sub)


# strptime cannot handle hour mins with no separator 
fix_string <- paste0(substr(string_check, 1,13), ':', substr(string_check, 14,15))

date_check <- as.POSIXct(strptime(fix_string, "%Y_%m_%d_%H:%M"), tz = 'Etc/GMT+6')

date_check_reduce <- unique(date_check)

# check data gaps against complete time series range

full_seq <- wxlogR::datetime_seq_full(date_check, timestep = )

length(full_seq) - length(date_check)

ndays_missing <- (length(full_seq) - length(date_check_reduce)) * (1/60) * (1/24)

df_check <- data.frame(datetime = date_check_reduce, val = 1)
df_full <- data.frame(datetime = full_seq)

df_compare <- left_join(df_full, df_check)

g <- ggplot(df_compare |> filter(datetime > as.POSIXct('2021-11-01'),
                            datetime < as.POSIXct('2022-10-01')), 
       aes(datetime, val)) +
  geom_line()

g
