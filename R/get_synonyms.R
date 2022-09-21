#' @name get_url_syno
#' @title Getting synonyms for retrieved taxonomic lists
#' @description
#' Lists of synonyms are scrapped from the species list.
#'
#' @param home Character value with the home url.
#' @param x A [cs_species-class] object of one row.
#'
#' @keywords internal
get_url_syno <- function(home, x) {
  Page <- htmlTreeParse(x$url, useInternalNodes = TRUE, encoding = "UTF-8")
  Link <- xpathSApply(Page, "//a/@href")
  idx <- grepl("sinonimoespecie", Link, fixed = TRUE)
  if (any(idx)) {
    Link <- Link[idx]
    Name <- xmlValue(xpathSApply(Page, paste0(
      '//a[@href="', Link,
      '"]/text()'
    )))
    return(cbind(
      TaxonConceptID = x$TaxonConceptID, usage = Name,
      url = paste0(home, Link)
    ))
  } else {
    return(NULL)
  }
}

#' @name get_synonyms
#' @rdname get_synonyms
#'
#' @title Get synonyms for retrieved species lists
#'
#' @description
#' Get synonyms from species lists retrieved by [conosur_species()].
#'
#' @param x A [cs_species-class] object.
#' @param progress A logical value indicating whether a progress bar should be
#'     displayed or not.
#' @param ... Further arguments passed among methods.
#'
#' @return
#' The input [cs_species-class] object with appended rows for synonyms.
#' An additional column **AcceptedName** is added indicating accepted names with
#' value `'TRUE'` and synonyms with value `'FALSE'`.
#'
#' @author Miguel Alvarez, \email{kamapu78@@gmail.com}
#' @examples
#' ## List for letter K
#' splist <- conosur_species(letter = "K")[1:5, ]
#' splist <- get_synonyms(splist)
#' head(splist)
#'
#' @export get_synonyms
get_synonyms <- function(x, ...) {
  UseMethod("get_synonyms", x)
}

#' @rdname get_synonyms
#' @aliases get_synonyms,cs_species-method
#' @method get_synonyms cs_species
#' @export
get_synonyms.cs_species <- function(x, progress = TRUE, ...) {
  home <- "http://www.darwin.edu.ar"
  if (nrow(x) == 1) {
    progress <- FALSE
  }
  query <- list()
  if (progress) {
    pb <- tkProgressBar(
      min = 0, max = nrow(x), width = 300,
      title = "Loading synonyms"
    )
    for (i in 1:nrow(x)) {
      Sys.sleep(0.1)
      setTkProgressBar(pb, i,
        label = paste0(
          i, " of ", nrow(x), " (",
          round(i / nrow(x) * 100, ),
          "% done)"
        )
      )
      query[[i]] <- get_url_syno(home, x[i, ])
    }
    close(pb)
  } else {
    for (i in 1:nrow(x)) {
      query[[i]] <- get_url_syno(home, x[i, ])
    }
  }
  # To data frame
  query <- as.data.frame(do.call(rbind, query))
  query$TaxonConceptID <- as.integer(query$TaxonConceptID)
  query$TaxonName <- dissect_name(query$usage, "  ", repaste = 1)
  query$AuthorName <- dissect_name(query$usage, "  ", repaste = 2)
  query$TaxonUsageID <- as.integer(sub(".*sincod=(.+)$", "\\1", query$url))
  query$AcceptedName <- FALSE
  x$AcceptedName <- TRUE
  x <- insert_rows(x, query[, names(query) != "usage"])
  class(x) <- c("cs_species", "data.frame")
  return(x)
}
