library(PEcAnRTM)
library(phytools)

########################################
# Paths
########################################

## Input

data_bm_base_in_path = "playground/spec_simulation/output/data_bm_base.rds"
data_ou_mono_in_path = "playground/spec_simulation/output/data_ou_mono.rds"
data_ou_poly_in_path = "playground/spec_simulation/output/data_ou_poly.rds"

tree_bm_base_in_path = "playground/spec_simulation/output/tree_base.rds"
tree_ou_mono_in_path = "playground/spec_simulation/output/tree_mono.rds"
tree_ou_poly_in_path = "playground/spec_simulation/output/tree_poly.rds"

regime_cols_in_path = "playground/spec_simulation/output/regime_cols.rds"


## Output

evolved_spectra_out_path = "playground/spec_simulation/figures/evolved_spectra_prospect.pdf"

prospect_spec_bm_base_out_path = "playground/spec_simulation/output/prospect_spec_bm_base.rds"
prospect_spec_ou_mono_out_path = "playground/spec_simulation/output/prospect_spec_ou_mono.rds"
prospect_spec_ou_poly_out_path = "playground/spec_simulation/output/prospect_spec_ou_poly.rds"


########################################
# Read simulated prospect param
########################################

data_bm_base = readRDS(data_bm_base_in_path)
data_ou_mono = readRDS(data_ou_mono_in_path)
data_ou_poly = readRDS(data_ou_poly_in_path)

tree_bm_base = readRDS(tree_bm_base_in_path)
tree_ou_mono = readRDS(tree_ou_mono_in_path)
tree_ou_poly = readRDS(tree_ou_poly_in_path)

regime_cols  = readRDS(regime_cols_in_path)

########################################
# Details
########################################

# Prospect
spec_wl_min = 400
spec_wl_max = 2500
spec_wl_vec = spec_wl_min : spec_wl_max

# Datasets
tip_names = ifelse(rownames(data_bm_base) == rownames(data_ou_mono) &
                   rownames(data_bm_base) == rownames(data_ou_poly),
                   rownames(data_bm_base),
                   stop("Tip names must be in the same order")
                   )

## Find tip states from simmap

# HACK -- Assumes that:
# 1) simmap is a painted tree in simmap format
# 2) tip edges have only one state!!!
tip_state_from_simmap = function(simmap){
    ntip     = Ntip(simmap)
    tips     = which(simmap$edge[ , 2] <= ntip)
    map_raw  = simmap$maps[ tips ]
    states   = sapply(map_raw, names)
    setNames(states, simmap$tip.label)
}


tip_state_ou_mono  = tip_state_from_simmap(tree_ou_mono)
tip_state_ou_poly  = tip_state_from_simmap(tree_ou_poly)

cols_state_ou_mono = regime_cols[tip_state_ou_mono]
cols_state_ou_poly = regime_cols[tip_state_ou_poly]

########################################
# Simulate spectra
########################################

# Convenience function
run_prospect_5_on_mat = function(param_mat, tips, wl_vec){
    dat = as.data.frame(t(param_mat))

    out = lapply(dat, function(x){
        y = PEcAnRTM::prospect( x, version = "5", include.wl = TRUE)
        y = y[ which( y[ , 3] == wl_vec ) , 1 ]
    })
    out = do.call(rbind, out)
    rownames(out) = tips
    colnames(out) = wl_vec
    out
}

# Simulate!
prospect_spec_bm_base = run_prospect_5_on_mat(data_bm_base, tip_names, spec_wl_vec)
prospect_spec_ou_mono = run_prospect_5_on_mat(data_ou_mono, tip_names, spec_wl_vec)
prospect_spec_ou_poly = run_prospect_5_on_mat(data_bm_base, tip_names, spec_wl_vec)


########################################
# Plot
########################################

pdf_sq     = c(6, 6)
pdf_wide   = c(9, 6)
pdf_s_wide = c(12, 6)
pdf_long   = c(6, 9)

pdf_size = pdf_s_wide

lty  = 1
ylab = "Reflectance"
xlab = "Wavelength"


pdf(evolved_spectra_out_path, width = pdf_size[1], height = pdf_size[2])

par(mfrow = c(1, 3))
matplot(x = spec_wl_vec, t(prospect_spec_bm_base), type = "l", col = "black", lty = lty, ylab = ylab, xlab = xlab)
matplot(x = spec_wl_vec, t(prospect_spec_ou_mono), type = "l", col = cols_state_ou_mono, lty = lty, ylab = ylab, xlab = xlab)
matplot(x = spec_wl_vec, t(prospect_spec_ou_poly), type = "l", col = cols_state_ou_poly, lty = lty, ylab = ylab, xlab = xlab)
dev.off()

########################################
# Write out
########################################

saveRDS(prospect_spec_bm_base, prospect_spec_bm_base_out_path)
saveRDS(prospect_spec_ou_mono, prospect_spec_ou_mono_out_path)
saveRDS(prospect_spec_ou_poly, prospect_spec_ou_poly_out_path)

# ################################################################################
# # Inversion
# ################################################################################
#
# invert_options = default.settings.prospect
#
# invert.options$n.tries      = 1
# invert.options$nchains      = 2
# invert.options$ngibbs       = 5000
# invert.options$burnin       = 1000
# invert.options$do.lsq.first = TRUE
#
#
# fit <- invert.lsq(observed = spec,
#                   inits = invert.options$inits(),
#                   invert.options$model,
#                   lower=invert.options$param.mins
# )
#
# prosp_inv = PEcAnRTM::invert.auto(spec,
#                                   invert.options = invert_options,
#                                   parallel        = TRUE)
#
# prosp =  PEcAnRTM::prospect(param = prosp_inv$results[1:5],
#                             version = "5")
#
# spec_minus_prosp = (spec - prosp[ , 1]) ^ 2
#
#
# par(mfrow = c(2, 1))
# plot(spec_wl_vec, spec, type = "l", col = "black")
# lines(spec_wl_vec, prosp[ , 1], col = "red")
# plot(spec_wl_vec, spec_minus_prosp, type = "l", col = "black")
# abline(v = stuff_loc_mu, col = "red")

