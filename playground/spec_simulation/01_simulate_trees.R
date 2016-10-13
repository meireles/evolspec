library(phytools)

########################################
# Paths
########################################

trees_fig_out_path = "playground/spec_simulation/figures/tree_mono_and_poly.pdf"

tree_base_out_path = "playground/spec_simulation/output/tree_base.rds"
tree_mono_out_path = "playground/spec_simulation/output/tree_mono.rds"
tree_poly_out_path = "playground/spec_simulation/output/tree_poly.rds"

regime_cols_out_path = "playground/spec_simulation/output/regime_cols.rds"

########################################
# Simulate
########################################

# Simulate base tree. Seed `11 seems to output a reasonable tree shape for our
# purposes
set.seed(11)

# Number of tips
n = 50

# Base tree
tree_base = phytools::pbtree(n = 50)

# Make painted tree with monophyletic `regime`
tree_mono = phytools::paintSubTree(tree_base,
                                   node      = 80,
                                   stem      = TRUE,
                                   state     = "deciduous",
                                   anc.state = "evergreen")

# And another tree with a convergent `regime`
tree_poly = phytools::paintSubTree(tree_base,
                                   node      = 90,
                                   stem      = TRUE,
                                   state     = "deciduous",
                                   anc.state = "evergreen")

tree_poly = phytools::paintSubTree(tree_poly,
                                   node      = 73,
                                   stem      = TRUE,
                                   state     = "deciduous",
                                   anc.state = "evergreen")

# Pick regime colors
regime_cols = setNames( c("orange", "green3"),
                        c("deciduous", "evergreen") )

########################################
# Plot trees to pdf
########################################

# Plot dimensions
pdf_sq     = c(6, 6)
pdf_wide   = c(9, 6)
pdf_s_wide = c(12, 6)
pdf_long   = c(6, 9)

# Pick a dimension
pdf_size = pdf_s_wide


pdf(trees_fig_out_path, width = pdf_size[1], height = pdf_size[2])

# Plot Trees
par(mfrow = c(1, 3))

lwd      = 2.75
fsize    = 1e-9
mar      = c(3, 1, 1, 1)
cex.axis = 0.75

phytools::plotTree(tree_base, lwd = lwd, fsize = fsize, mar = mar)
phytools::plotSimmap(tree_mono, colors = regime_cols, lwd = lwd, fsize = fsize, mar = mar)
phytools::plotSimmap(tree_poly, colors = regime_cols, lwd = lwd, fsize = fsize, mar = mar)

dev.off()

########################################
# Write data out
########################################

saveRDS(tree_base, file = tree_base_out_path)
saveRDS(tree_mono, file = tree_mono_out_path)
saveRDS(tree_poly, file = tree_poly_out_path)

saveRDS(regime_cols, file = regime_cols_out_path)

