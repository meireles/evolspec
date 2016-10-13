library("devtools")
devtools::use_package("phytools")
devtools::use_package("Rphylopars")

################################################################################
# Public function
################################################################################

#' Fit evolutionary model to spectra
#'
#' @param tree A phylogeny of class "phylo"
#' @param spec A spectra dataset, where the first column are species names and
#' the remaining are data.
#' @param model One of the following models: "BM" or "OU"
#'
#' @return
#' @export
fit_spec_evol = function(tree, spec, model = "BM", ...){

    if(model == "BM"){
        phylopars_result = i_fit_spec_evol_rphylopars(tree = tree,
                                                      spec = spec,
                                                      model = model,
                                                      ...)
    } else {
        message("Sorry, model not implemented yet. Returning NULL")
        phylopars_result = NULL
    }
    return(phylopars_result)
}

################################################################################
# Internal functions
################################################################################

i_fit_spec_evol_rphylopars = function(tree, spec, model = "BM", ...) {

    fit = Rphylopars::phylopars(trait_data  = spec,
                                tree        = tree,
                                model       = model,
                                full_alpha  = FALSE,
                                REML        = FALSE,
                                pheno_error = FALSE,
                                phylo_correlated = FALSE,
                                pheno_correlated = FALSE,
                                ...)
    return(fit)
}
