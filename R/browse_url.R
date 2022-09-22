#' @name browse_url
#' @rdname browse_url
#' @title Load species page in browser
#'
#' @description
#' Load the respective page of a species or taxon into the default browser.
#' This is a wrapper of [browseURL()].
#'
#' @param x A [cs_species-class] object.
#' @param maxsum An integer value indicating the maximum number of URLs to be
#'     opened at once. This parameter avoids opening too much tabs in your
#'     browser.
#' @param ... Further arguments passed to [browseURL()].
#'
#' @export get_info
browse_url <- function(x, ...) {
  UseMethod("browse_url", x)
}

#' @rdname browse_url
#' @aliases browse_url,cs_species-method
#' @method browse_url cs_species
#' @export
browse_url.cs_species <- function(x, maxsum = 10, ...) {
  maxsum <- min(c(nrow(x), maxsum))
  for (i in 1:maxsum) {
    browseURL(x$url[i], ...)
  }
}
