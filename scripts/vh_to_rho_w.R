# script to handle non-linear adjustment to hygrometer output

library(wxlogR)

raw_path <- 'data/hi-freq/raw/subset-15min-ascii/'
out_path <- 'data/hi-freq/clean/subset-15min-ascii-calcrho/'

l <- list.files('data/hi-freq/raw/subset-15min-ascii', full.names = F)

# constants from logger program

XKW_high <- -0.181 # sensor specific S/N 1527, scaled, dry vapour range [m^3 / (g cm)]
x <- 1  # Path length of KH20 (cm)
xkw <- (-1) * x * XKW_high  # Path Length * Water Vapor absorbtion coefficient (m^3 / g)

df <- wxlogR::load_CS_1000(paste0(raw_path, l[1]), timezone = 'GMT-6')

df$rho_w <- log(df$vh) / xkw # g / m3

calc_rho_w <- function(file) {
  df <- wxlogR::load_CS_1000(paste0(raw_path, file), timezone = 'GMT-6')
  
  df$rho_w <- log(df$vh) / xkw
  
  df <- df[c('Ux', 'Uy', 'Uz', 'Ts', 'rho_w')]
  
  write.csv(df, paste0(out_path, file), row.names = F)
}

lapply(l, calc_rho_w)

df_out <- read.csv(paste0(out_path, l[1]))
