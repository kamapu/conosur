#' @name cs_species-class
#'
#' @title List of species
#'
#' @description
#' An S3 class for species lists retrieved by [conosur_species()], which
#' inherits properties of data frames.
#'
#' @exportClass cs_species
setOldClass(c("cs_species", "data.frame"))
