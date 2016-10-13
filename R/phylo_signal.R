library("devtools")

devtools::use_package("phytools")
devtools::use_package("Rphylopars")

################################################################################
# Public function
################################################################################

#' Estimates phylogenetic signal on spectra
#'
#' @param tree A phylogeny of class "phylo"
#' @param spec A matrix with spectral data, where the first column is are species
#' names that match tip names of the tree.
#' @param method  Either "lambda" (Pagel's) or "K" (Bloomberg's).
#' @param by_band Boolean. The default TRUE estimates signal for each wavelength
#' independently. If FALSE, signal is estimated for the spectrum as a whole using
#' method = "lambda" ("K" not availiable if by_band = FALSE).
#'
#' @param se Optional. A matrix of spectral standard erros returned by
#' aggregate_by_species`
#'
#' @return A two column data.frame with the estimated signal in the first column
#' and a p-value from a significance test in the second column.
#'
#' @export
phylo_signal = function(tree, spec, method = c("lambda", "K"), by_band = TRUE, se = NULL) {

    if(by_band){
        out = i_phylo_signal_by_band(tree, spec, method, se)
    } else {
        message("method set to 'lambda'")
        out = i_phylo_signal_joint(tree, spec)
    }
    return(out)
}

################################################################################
# Internal functions
################################################################################

i_phylo_signal_by_band = function(tree, spec, method, se = NULL, ...) {
    require("phytools")

    # assumes that the first column are species names
    x        = spec[ , -1 ]
    sp_names = spec[ ,  1 ]
    nbands   = ncol(x)

    if( ! is.null(se) ){
        x_se     = se[ , -1]
    }

    signal_mu  = rep(NA, nbands )
    signal_p   = rep(NA, nbands )

    for(i in 1:nbands) {
        d = setNames(x[ , i], sp_names)

        if( ! is.null(se) ){
            s = setNames(x_se[ , i], sp_names)
        } else{
            s = NULL
        }

        p = phytools::phylosig(tree,
                               x = d,
                               test = TRUE,
                               method = method,
                               nsim = 199,
                               se = s,
                               ...)
        signal_mu[i] = p[[1]]
        signal_p[i]  = p$P
    }

    result           = data.frame(signal_mu, signal_p)
    colnames(result) = c(method, "pval")
    return(result)
}


i_phylo_signal_joint = function(tree, spec) {

    message("Sorry, not implemented yet. Returning NULL")
    return(NULL)
    # require("Rphylopars")
    #
    # lambda_phy  = phylopars(trait_data = spec, tree = tree, model = "lambda")
    # lambda_star = phylopars(trait_data = spec, tree = tree, model = "star")
    #
    # chi_sq = as.double( 2*(logLik(lambda_phy) - logLik(lambda_star)) )
    # dof    = lambda_phy$npars - lambda_star$npars
    # pval   = pchisq(q = chi_sq, df = dof, lower.tail = FALSE)
    #
    # result = data.frame(lambda = lambda_phy$model$lambda,
    #                     pval   = pval)
    # return(result)
}
