library("devtools")
library("ape")

########################################
# Paths
########################################

oak_tree_path = "data-raw/oak_spectra_phylogeny.tre"
oak_spec_path = "data-raw/oak_spectra_dataset.csv"

########################################
# Read data
########################################

oak_tree         = ape::read.tree(oak_tree_path)
oak_spec_jcb2016 = read.csv(oak_spec_path, header = TRUE, check.names = FALSE)


########################################
# Process data
########################################

## spectra
# for some reason, check.names seems to have no effect here. clean up column names

cols_keep = c("Species", paste("X", as.character( 400:2400 ), sep = ""))
col_names = c("species", as.character( 400:2400 ))

oak_spec_jcb2016           = oak_spec_jcb2016[ , cols_keep]
colnames(oak_spec_jcb2016) = col_names

########################################
# Write data out
########################################

devtools::use_data(oak_tree, overwrite = TRUE)
devtools::use_data(oak_spec_jcb2016, overwrite = TRUE)
