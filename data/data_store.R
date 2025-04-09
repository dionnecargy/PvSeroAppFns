# WEHI standards MFI data
all_stds_MFI <- read.csv("data/all_stds_MFI.csv")
usethis::use_data(all_stds_MFI)

# WEHI longitudinal samples training MFI data
longitudinal_MFI <- read.csv("data/longitudinal_MFI.csv")
usethis::use_data(longitudinal_MFI)

# WEHI longitudinal samples training RAU data
longitudinal_RAU <- read.csv("data/longitudinal_RAU.csv")
usethis::use_data(longitudinal_RAU)

# WEHI PNG and ETH standards data
png_eth_stds <- read.csv("data/png_eth_stds.csv")
usethis::use_data(png_eth_stds)

# Model Threshold Values data
threshold_values <- read.csv("data/threshold_values.csv")
usethis::use_data(threshold_values)

# Model without LF016 Threshold Values data
excluding_LF016_threshold_values <- read.csv("data/excluding_LF016_threshold_values.csv")
usethis::use_data(excluding_LF016_threshold_values)

# Platemap data
platemap <- read.csv("data/platemap.csv")
usethis::use_data(platemap)
