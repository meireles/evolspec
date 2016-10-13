library(evolspec)
library(ape)

spec_orig = read.csv("data-raw/chem16_ecosis_mod.csv", check.names = F)


spec_norm =

spec_agr  = aggregate_by_species(spec_orig)
spec_mu   = spec_agr$mean
spec_mu$species = gsub(" ", "_", as.character(spec_mu$species))
sp_list   = spec_mu$species

tree_ck_orig    = read.tree("data-raw/big_bio.tre")
tree_zanne_orig = read.tree("data-raw/zanne_tree_subset.tre")

sp_list_match = intersect(sp_list, tree_zanne_orig$tip.label)

spec = spec_mu[ spec_mu$species %in% sp_list_match , ]
tree = drop.tip(tree_zanne_orig,
                setdiff(tree_zanne_orig$tip.label, sp_list_match)
)

wlavelengths    = colnames(spec)[-1]



### normalized vector
vec_norm_spec = function(x){
    result = t(apply(x[ , -1], 1, function(y){y / sum(abs(y))}  ))
    result =  data.frame(species = x[ , 1], result, check.names = FALSE )
}

spec_norm = vec_norm_spec(spec)


spec_phylosig_lambda = evolspec::phylo_signal(tree, spec_norm,
                                              method = "lambda")

spec_phylosig_k = evolspec::phylo_signal(tree, spec_norm,
                                         method = "K")





par(mfrow = c(1, 2))
plot(x = wlavelengths,
     y = spec_phylosig_lambda$lambda,
     ylab = "lambda", main = "lambda",
     cex = 0.5, cex.axis = 0.75,
     col = ifelse(spec_phylosig_lambda$pval <= 0.05, "red", "black"))
plot(x = wlavelengths,
     y = spec_phylosig_k$K,
     ylab = "K", main = "K",
     cex = 0.5, cex.axis = 0.75,
     col = ifelse(spec_phylosig_k$pval <= 0.05, "red", "black"))


# Drop one oak species from the tree
drop      = "Acer_saccharum"
tree_drop = ape::drop.tip(tree, tip = drop)

spec_resample = spec_norm[ , c("species", as.character( seq(400, 2400, by = 100) ) )]

########################################
# Maximum Likelihood
########################################


find_oak_ml = evolspec::place_spec_on_tree(tree  = tree_drop,
                                           spec  = spec,
                                           model = "BM",
                                           method = "ml")



