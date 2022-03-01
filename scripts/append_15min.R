# combine all 15 min EC downloads

ec1 <- wxlogR::load_CS_1000(path = '../../field-downloads/fortress/hanging tree/hi freq/clean/TOA5_FRG_20211214.flux15.dat',
                            skip_4_dat = 4)

ec2 <- wxlogR::load_CS_1000(path = '../../field-downloads/fortress/hanging tree/hi freq/clean/TOA5_FRG_20220127.flux15.dat',
                            skip_4_dat = 4)

ec3 <- wxlogR::load_CS_1000(path = '../../field-downloads/fortress/hanging tree/hi freq/clean/TOA5_FRG_low_20220214.flux15.dat',
                            skip_4_dat = 4)

out <- rbind(ec1, ec2, ec3)

write.csv(out, 'data/eddy_cov_15min_combined.csv')
saveRDS(out, 'data/eddy_cov_15min_combined.rds')
