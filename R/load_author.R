#' @name get_author
#' @title Appends authorships to existing lists of species
#' @description
#' Author names need to be retrieved from the specific pages of species in an
#' additional step. This function is called internally.
#'
#' @param x A [cs_species-class] object of one row.
#'
#' @keywords internal
get_author <- function(x) {
  Table <- htmlTreeParse(x$url,
    useInternalNodes = TRUE,
    encoding = "UTF-8"
  )
  Table <- readHTMLTable(Table, stringsAsFactors = FALSE)[[1]]
  Table <- Table[!is.na(Table[, 1]), ]
  # For sensu stricto
  if (x$Level != "species" & x[, x$Level, drop = TRUE] == x$species) {
    x$Level <- "species"
  }
  x$Level <- c("Sigla sp.", "Sigla ssp.", "Sigla var.", "Sigla f.")[
    match(x$Level, c("species", "subspecies", "variety", "form"))
  ]
  return(Table[Table[, 1] == x$Level, 2])
}

#' @name load_author
#' @rdname load_author
#'
#' @title Add authorship to a species list
#'
#' @description
#' Add author names of species lists loaded by [conosur_species()].
#'
#' @param x A [cs_species-class] object.
#' @param progress A logical value indicating whether a progress bar should be
#'     displayed or not.
#' @param ... Further arguments passed among methods.
#'
#' @return
#' A [cs_species-class] object with an additional column named `'AuthorName'`.
#' @author Miguel Alvarez, \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## List for letter A
#' splist <- conosur_species(letter = "K")[1:5, ]
#' splist <- load_author(splist)
#'
#' @export load_author
load_author <- function(x, ...) {
  UseMethod("load_author", x)
}

#' @rdname load_author
#' @aliases load_author,cs_species-method
#' @method load_author cs_species
#' @export
load_author.cs_species <- function(x, progress = TRUE, ...) {
  if (nrow(x) == 1) {
    progress <- FALSE
  }
  x$AuthorName <- NA
  if (progress) {
    pb <- tkProgressBar(
      min = 0, max = nrow(x), width = 300,
      title = "Loading authors"
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
      x$AuthorName[i] <- get_author(x[i, ])
    }
    close(pb)
  } else {
    for (i in 1:nrow(x)) {
      x$AuthorName[i] <- get_author(x[i, ])
    }
  }
  x$AuthorName <- iconv(x$AuthorName, "UTF-8", "UTF-8")
  return(x)
}
