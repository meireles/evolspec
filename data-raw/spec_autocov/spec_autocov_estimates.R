
library("devtools")

########################################
# Paths
########################################

spec_autocov_asd_path = "data-raw/spec_autocov_estimate_asd.csv"

########################################
# Read data
########################################

spec_autocov_estimate = read.csv(spec_autocov_asd_path,
                                 header = TRUE, check.names = F, row.names = 1)

########################################
# Write data out
########################################

devtools::use_data(spec_autocov_estimate, overwrite = TRUE)
