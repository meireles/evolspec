#' Covariance between all spectral bands
#'
#' Holds the estimated full symmetric covariance matrix of spectral bands from
#' an ASD dataset including broadleaf and needle leaf species.
#'
#' @format Matrix of dimension 2001 by 2001. Covariance between bands 400 to 2400
#' at 1nm resolution.
#'
#' @source Phil Townsend \url{http://labs.russell.wisc.edu/townsend/phil-townsend/}
"spec_autocov_estimate"


#' Oaks of North America spectral data
#'
#' ASD spectral measurements of Oaks grown in a greenhouse at UMN.
#'
#' @format Matrix where the forst column are species names and the remaining 2001 columns are
#' wavelengths 400 to 2400 nm. Spectral resolution = 1nm.
#'
#' @source Jeannine Cavender-Bares. **todo**{link to paper and dryad}
"oak_spec_jcb2016"


#' Phylogeny for Oaks of North America
#'
#' Part of the Oak time calibrated tree from Hipp et al. (unpublished).
#'
#' @format An object of class "phylo" ("ape" package).
#'
#' @source Hipp et al.
"oak_tree"


