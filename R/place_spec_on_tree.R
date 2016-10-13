library("devtools")

devtools::use_package("phytools")
devtools::use_package("Rphylopars")

################################################################################
# Public function
################################################################################

#' Places an ulabebed specrum on a tree
#'
#' @param tree Tree of class "phylo".
#' @param spec pectral dataset, where the first column are species names and
#' the remaining are data. The spectra with unknown placement must be in this
#' @param model Only "BM" availiable for now.
#' @param method Choose "ml" or "bayes"
#'
#' @return
#' @export
#'
#' @examples
place_spec_on_tree = function(tree, spec, model = "BM", method = "ml") {
    if(method == "ml"){
        result = i_place_spec_on_tree_ml(tree = tree, spec = spec, model = model)
        return(result)
    } else {
        message("Not implemented yet. Returning NULL")
        return(NULL)
    }

}

################################################################################
# internal functions
################################################################################

#' Place specrum on tree using maximum likelihood
#'
#' @param tree Tree of class "phylo".
#' @param spec Spectral dataset, where the first column are species names and
#' the remaining are data. The spectra with unknown placement must be in this
#' data.
#' @param model Choose "BM" or ...
#' @param mc_cores How many cores to run this on
#'
#' @return
#' @export
i_place_spec_on_tree_ml = function(tree, spec, model = "BM", mc_cores=1){

    new_tip   = setdiff(spec[ , 1], tree$tip.label)
    trees     = i_add_tip_everywhere(base_tree = tree, new_tip)
    fits      = NA
    if(mc_cores==1) {
        fits      = lapply(trees, fit_spec_evol, spec = spec, model = model)
    } else {
        fits      = mclapply(trees, fit_spec_evol, spec = spec, model = model, mc.cores=mc_cores)
    }
    logliks   = sapply(fits, function(x){ as.vector(x$logLik)} )

    best_place = names(logliks)[ which(max(logliks) == logliks) ]
    new_tree   = trees[[best_place]]

    return(list(new_tree       = new_tree,
                base_tree      = tree,
                lnlik_by_node  = logliks,
                best_placement = best_place
                ))
}



#' Add a tip to every branch on a phylogeny
#'
#' @param base_tree Tree of class "phylo" where the new tip will be grafted.
#' @param tip_label Name of the new tip. Must be different from the tip labels
#' in `base_tree`
#'
#' @return
i_add_tip_everywhere = function(base_tree, tip_label){

    require("phytools")

    nodes   = base_tree$edge[ , 2]
    bl_half = base_tree$edge.length * 0.5  # graft at half branch lengths

    if(tip_label %in% base_tree$tip.label){
        stop("Invalid tip_label: another tip on base_tree already has that name.")
    }

    f = function(where, pos) {
        result = phytools::bind.tip(tree      = base_tree,
                                    tip.label = tip_label,
                                    where     = where,
                                    position  = pos)
        class(result) = "phylo"
        return(result)
    }
    trees        = mapply(f, nodes, bl_half, SIMPLIFY = FALSE)
    names(trees) = nodes
    return(trees)
}
