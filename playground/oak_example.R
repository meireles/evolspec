library(evolspec)
library(Rphylopars)
library(phytools)

par_orig = par()

########################################
# Data
########################################

oak_spec_only  = oak_spec_jcb2016[ , -1]
wlavelengths   = colnames(oak_spec_only)

## normalize individual spectra to sum to unit.
vec_norm_spec = function(x) {
    result = t(apply(x[ , -1], 1, function(y){y / sum(abs(y))}  ))
    result =  data.frame(species = x[ , 1], result, check.names = FALSE )
}

oak_spec_pick  = vec_norm_spec(oak_spec_jcb2016) # oak_spec_jcb2016

oak_spec_by_sp = evolspec::aggregate_by_species(oak_spec_pick)

by = 20
wl_trim                = as.character( seq(400, 2400, by = by) )
oak_spec_trim          = oak_spec_jcb2016[ , c("species", wl_trim)]
oak_spec_only_trim     = oak_spec_only[ , wl_trim ]
oak_spec_by_sp_trim_mu = evolspec::aggregate_by_species(oak_spec_trim)$mean

spec_cov_trim = evolspec::spec_autocov_estimate[ , wl_trim ]
spec_cov_trim = spec_cov_trim[ wl_trim , ]


########################################
# Phylogenetic signal
########################################

spec_phylosig_lambda = evolspec::phylo_signal(oak_tree,
                                              oak_spec_by_sp$mean,
                                              method = "lambda",
                                              se = NULL)      # oak_spec_by_sp$se

spec_phylosig_k = evolspec::phylo_signal(oak_tree,
                                         oak_spec_by_sp$mean,
                                         method = "K",
                                         se = NULL)           # oak_spec_by_sp$se

par(mfrow = c(1, 2))
plot(x = wlavelengths,
     y = spec_phylosig_lambda$lambda,
     ylab = "lambda", main = "Pagel's lambda",
     cex = 0.5, cex.axis = 0.75,
     col = ifelse(spec_phylosig_lambda$pval <= 0.05, "red", "black"))
plot(x = wlavelengths,
     y = spec_phylosig_k$K,
     ylab = "K", main = "Blombergs's K",
     cex = 0.5, cex.axis = 0.75,
     col = ifelse(spec_phylosig_k$pval <= 0.05, "red", "black"))


################################################################################
# Brownian rates of evolution
################################################################################

fit   = fit_spec_evol(oak_tree, oak_spec_by_sp_trim_mu, model = "BM")
rates = diag(fit$pars$phylocov)

plot(x = names(rates),
     y = rates,
     ylab = "sigma", main = "Brownian rates of evolution",
     cex = 1.2, cex.axis = 0.75,
     col = "black", pch = 16)


################################################################################
# Placing on the tree
################################################################################

# Drop one oak species from the tree
drop = "quercus_macrocarpa"
# "quercus_texana"  # "quercus_rubra"    # "quercus_macrocarpa"

oak_tree_drop = ape::drop.tip(oak_tree, tip = drop)


########################################
# Maximum Likelihood
########################################

find_oak_ml = evolspec::place_spec_on_tree(tree  = oak_tree_drop,
                                           spec  = oak_spec_by_sp_trim_mu,
                                           model = "BM",
                                           method = "ml")

## Plot
par = par_orig
par(mfrow = c(1, 3))

plotBranchbyTrait(find_oak_ml$base_tree, find_oak_ml$lnlik_by_node,
                  mode = "edges", cex = 0.75)

plotTree(oak_tree, cex = 0.75)
add.arrow(oak_tree, drop, col = "red", cex = 2)

plotTree(find_oak_ml$new_tree, cex = 0.75)
add.arrow(find_oak_ml$new_tree, drop, col = "red", cex = 2)

