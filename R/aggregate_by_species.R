#' Aggregate spectra by species
#'
#' @param spec Spectra matrix where the first column is labeled "species" and
#' holds species names, and the remaining columns are wavelengths.
#'
#' @return List of 3 matrices: "mean", "sd" for standard deviation and "se" for standard error
#' @export
aggregate_by_species = function(spec){
    mean = aggregate( . ~ species, spec, mean, na.rm = TRUE)
    sd   = aggregate( . ~ species, spec, sd, na.rm = TRUE)

    sq_n = sqrt(table(spec$species))

    se   = sd[ , -1] / sq_n
    se   = cbind(species = sd[ , 1], se)

    list(mean = mean, sd = sd, se = se)
}
