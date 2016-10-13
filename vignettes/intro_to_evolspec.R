## ---- eval=FALSE---------------------------------------------------------
#  # If you don't already have `devtools`
#  install.packages("devtools")
#  
#  library("devtools")
#  
#  # Install `evolspec`
#  devtools::install_github("meireles/evolspec")

## ------------------------------------------------------------------------
library(evolspec)

## The oak_spec_dataet has "species" as the first column.
oak_spec_jcb2016[ 1:3, 1:5 ]

## ------------------------------------------------------------------------
oak_spec_by_sp = evolspec::aggregate_by_species(oak_spec_jcb2016)
names(oak_spec_by_sp)

## ------------------------------------------------------------------------
## To get a matrix with spectra only and get the wavelength labels, do
oak_spec_only = oak_spec_jcb2016[ , -1]
wlavelengths  = colnames(oak_spec_only)

## You can also resample the original oak data at a certain resolution,
## for instance, 10 nm
wl_subset_10nm     = as.character( seq(400, 2400, by = 10) )
oak_spec_10nm      = oak_spec_jcb2016[ , c("species", wl_subset_10nm)]
oak_spec_only_10nm = oak_spec_only[ , wl_subset_10nm ]

## Finally you can also aggregate by species the resamples spectra at 10nm resolution
oak_spec_by_sp_10nm_mu =  evolspec::aggregate_by_species(oak_spec_10nm)$mean

## ------------------------------------------------------------------------
library("evolspec")

plot(oak_tree, cex = 0.5)

## ------------------------------------------------------------------------
spec_phylosig_lambda = evolspec::phylo_signal(oak_tree,
                                              oak_spec_by_sp$mean,
                                              method = "lambda")

spec_phylosig_k      = evolspec::phylo_signal(oak_tree,
                                              oak_spec_by_sp$mean,
                                              method = "K")

par(mfrow = c(1, 2))

plot(x = wlavelengths,
     y = spec_phylosig_lambda$lambda,
     ylab = "Pagel's lambda", cex = 0.5,
     col = ifelse(spec_phylosig_lambda$pval <= 0.05, "red", "black"))

plot(x = wlavelengths,
     y = spec_phylosig_k$K,
     ylab = "Bloombergs's K", cex = 0.5,
     col = ifelse(spec_phylosig_k$pval <= 0.05, "red", "black"))


