library(mvMORPH)
library(phytools)

########################################
# Paths
########################################

## Input
tree_base_in_path = "playground/spec_simulation/output/tree_base.rds"
tree_mono_in_path = "playground/spec_simulation/output/tree_mono.rds"
tree_poly_in_path = "playground/spec_simulation/output/tree_poly.rds"
regime_cols_in_path = "playground/spec_simulation/output/regime_cols.rds"

## Output

traitigram_Cm_out_path  = "playground/spec_simulation/figures/traitigram_prosp_Cm.pdf"
traitigram_Cab_out_path = "playground/spec_simulation/figures/traitigram_prosp_Cab.pdf"

data_bm_base_out_path = "playground/spec_simulation/output/data_bm_base.rds"
data_ou_mono_out_path = "playground/spec_simulation/output/data_ou_mono.rds"
data_ou_poly_out_path = "playground/spec_simulation/output/data_ou_poly.rds"


########################################
# Read Trees and color data
########################################

tree_base   = readRDS(tree_base_in_path)
tree_mono   = readRDS(tree_mono_in_path)
tree_poly   = readRDS(tree_poly_in_path)
regime_cols = readRDS(regime_cols_in_path)

########################################
# Trait Simulation
########################################

# Range of values taken from http://opticleaf.ipgp.fr/index.php?page=prospect
prospect_param_basev = c("N"   = 1.6,        # [1.0 - 3.0]
                         "Cab" = 30.0,       # [0.0 - 100.0] ug cm^-2
                         "Car" = 15.0,       # [0.0 - 25.0]  ug cm^-2
                         "Cw"  = 0.01,       # [0.0 - 0.05]  cm
                         "Cm"  = 0.009)      # [0.0 - 0.02]  g cm^-2

prospect_sigma_basev = diag( c(0.005, 0.2, 0.2, 1e-6, 4e-8) )

prospect_param_names = names(prospect_param_basev)
n_prospect_param     = length(prospect_param_names)


########################################
# Seed (for both BM and OU models)
########################################

set.seed(12)

########################################
# BM
########################################

theta_bm = prospect_param_basev
sigma_bm = prospect_sigma_basev

bm_param = list(ntraits = n_prospect_param,
                sigma   = sigma_bm,
                theta   = theta_bm,
                names_traits = prospect_param_names)


data_bm_base  = mvMORPH::mvSIM(tree_mono,
                               param = bm_param,
                               model = "BM1",
                               nsim = 1)

########################################
# OU
########################################

theta_ou_evg = prospect_param_basev
theta_ou_dec = prospect_param_basev


theta_ou_dec["Cm"] = 0.004

theta_ou = c(rbind(theta_ou_evg, theta_ou_dec))

alpha_ou = matrix(c(0.2, 0.0, 0.0, 0.0, 0.0,
                    0.0, 0.2, 0.0, 0.0, 0.0,
                    0.0, 0.0, 0.2, 0.0, 0.0,
                    0.0, 0.0, 0.0, 0.2, 0.0,
                    0.0, 0.0, 0.0, 0.0, 0.08),   # Cm with weaker alpha
                  n_prospect_param)

# Rate of evolution
sigma_ou = prospect_sigma_basev

# Multivariate OU parameters
mv_ou_param = list(ntraits = n_prospect_param,
                   sigma   = sigma_ou,
                   alpha   = alpha_ou,
                   theta   = theta_ou,
                   names_traits = prospect_param_names)

data_ou_mono = mvMORPH::mvSIM(tree_mono, param = mv_ou_param, model="OUM", nsim = 1)
data_ou_poly = mvMORPH::mvSIM(tree_poly, param = mv_ou_param, model="OUM", nsim = 1)


########################################
# Plots
########################################

pdf_sq     = c(6, 6)
pdf_wide   = c(9, 6)
pdf_s_wide = c(12, 6)
pdf_long   = c(6, 9)

pdf_size = pdf_s_wide

ylim   = NULL #c(0.006, 0.01)
fsize  = 0.02
spread = FALSE

####################
## Cm (LMA)
####################
pdf(traitigram_Cm_out_path, width = pdf_size[1] , height = pdf_size[2])
trait_plot = "Cm"
par(mfrow = c(1, 3))

phytools::phenogram(tree = tree_base,
                    x    = data_bm_base[ , trait_plot],
                    ylim = ylim,
                    fsize = fsize,
                    spread.labels = spread,
                    colors = "black")
phytools::phenogram(tree = tree_mono,
                    x    = data_ou_mono[ , trait_plot],
                    ylim = ylim,
                    fsize = fsize,
                    spread.labels = spread,
                    colors = regime_cols)
phytools::phenogram(tree = tree_poly,
                    x    = data_ou_poly[ , trait_plot],
                    ylim = ylim,
                    fsize = fsize,
                    spread.labels = spread,
                    colors = regime_cols)

title(main = "Cm (LMA) PROSPECT param", outer = TRUE)

dev.off()


####################
## Cab
####################
pdf(traitigram_Cab_out_path, width = pdf_size[1] , height = pdf_size[2])
trait_plot = "Cab"
par(mfrow = c(1, 3))

phytools::phenogram(tree = tree_base,
                    x    = data_bm_base[ , trait_plot],
                    ylim = ylim,
                    fsize = fsize,
                    spread.labels = spread,
                    colors = "black")
phytools::phenogram(tree = tree_mono,
                    x    = data_ou_mono[ , trait_plot],
                    ylim = ylim,
                    fsize = fsize,
                    spread.labels = spread,
                    colors = regime_cols)
phytools::phenogram(tree = tree_poly,
                    x    = data_ou_poly[ , trait_plot],
                    ylim = ylim,
                    fsize = fsize,
                    spread.labels = spread,
                    colors = regime_cols)

title(main = "Cab (Chlorophyll a + b) PROSPECT param", outer = TRUE)

dev.off()

########################################
# Fit Models
########################################

# BMM_base = mvMORPH::mvBM(tree_base, data_bm_base, model = "BMM")
# OUM_mono = mvMORPH::mvOU(tree_mono, data_ou_mono, model = "OUM")
# OUM_poly = mvMORPH::mvOU(tree_poly, data_ou_poly, model = "OUM")

# cov2cor(stationary(BMM_base))
# cov2cor(stationary(OUM_mono))
# cov2cor(stationary(OUM_mono))


########################################
# Write data out
########################################

saveRDS(data_bm_base, data_bm_base_out_path)
saveRDS(data_ou_mono, data_ou_mono_out_path)
saveRDS(data_ou_poly, data_ou_poly_out_path)

