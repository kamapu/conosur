#' @name get_table
#' @title Appends attributes to an existing lists of species
#' @description
#' Author names need to be retrieved from the specific pages of species in an
#' additional step. This function is called internally.
#'
#' @param x A [cs_species-class] object of one row.
#'
#' @keywords internal
get_table <- function(x) {
  tryCatch(
    {
      OUT <- NULL
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
      OUT <- c(
        as.integer(x$TaxonConceptID),
        Table[match(
          c(x$Level, "Status", "H\u00e1bito", "Elevaci\u00f3n (m s.m.)"),
          Table[, 1]
        ), 2]
      )
    },
    error = function(e) {
      message(paste0(
        "Error at concept '",
        x$TaxonConceptID, "'!"
      ))
    },
    finally = return(OUT)
  )
}

#' @name get_info
#' @rdname get_info
#'
#' @title Add authorship and attributes to a species list
#'
#' @description
#' Author names and species attributes of species lists downloaded by
#' [conosur_species()] need to be requested in specific pages.
#'
#' @param x A [cs_species-class] object.
#' @param subset An expression selecting a subset of rows for retrieving the
#'     information. This can be useful if a previous run had failed in some
#'     species.
#' @param progress A logical value indicating whether a progress bar should be
#'     displayed or not.
#' @param ... Further arguments passed among methods.
#'
#' @return
#' A [cs_species-class] object with additional columns named **AuthorName**
#' (species name authorship), **status** (native or non-native), **life_form**,
#' **elevation_min** and **elevation_max**.
#'
#' @author Miguel Alvarez, \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## List for letter K
#' splist <- conosur_species(letter = "K")[1:5, ]
#' splist <- get_info(splist)
#'
#' @export get_info
get_info <- function(x, ...) {
  UseMethod("get_info", x)
}

#' @rdname get_info
#' @aliases get_info,cs_species-method
#' @method get_info cs_species
#' @export
get_info.cs_species <- function(x, subset, progress = TRUE, ...) {
  if (!missing(subset)) {
    idx <- substitute(subset)
    idx <- eval(idx, x, parent.frame())
    x_in <- x
    x <- x[idx, ]
  }
  if (nrow(x) == 1) {
    progress <- FALSE
  }
  sp_info <- list()
  if (progress) {
    pb <- tkProgressBar(
      min = 0, max = nrow(x), width = 300,
      title = "Loading species info"
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
      sp_info[[i]] <- get_table(x[i, ])
    }
    close(pb)
  } else {
    for (i in 1:nrow(x)) {
      sp_info[[i]] <- get_table(x[i, ])
    }
  }
  sp_info <- as.data.frame(do.call(rbind, sp_info))
  names(sp_info) <- c(
    "TaxonConceptID", "AuthorName", "status", "life_form",
    "elevation"
  )
  sp_info$TaxonConceptID <- as.integer(sp_info$TaxonConceptID)
  ele <- gsub(".*?([0-9]+).*", "\\1", str_split(sp_info$elevation, " - ",
    simplify = TRUE
  ))
  colnames(ele) <- c("elevation_min", "elevation_max")
  sp_info$elevation_min <- as.integer(ele[, "elevation_min"])
  sp_info$elevation_max <- as.integer(ele[, "elevation_max"])
  sp_info <- sp_info[, names(sp_info) != "elevation"]
  sp_info$AuthorName <- iconv(sp_info$AuthorName, "UTF-8", "UTF-8")
  if (!missing(subset)) {
    x <- x_in
  }
  for (i in names(sp_info)[names(sp_info) != "TaxonConceptID"]) {
    if (!i %in% names(x)) {
      x[, i] <- NA
    }
    x[, i] <- replace_idx(x[, i],
      idx1 = x$TaxonConceptID,
      idx2 = sp_info$TaxonConceptID, new = sp_info[, i, drop = TRUE]
    )
  }
  # class(x) <- c("cs_species", "data.frame")
  return(x)
}
