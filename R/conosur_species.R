#' @name get_url
#' @title Get url for the species list
#' @description
#' In the Flora del Conosur species are listed in single pages by letters
#' (initials). A first step to retrieve the list is getting the respective
#' URLs.
#'
#' @param home Character value with the URL for the main site.
#' @param letter Character vector with the requested initials.
#'
#' @keywords internal
get_url <- function(home, letter) {
  Link <- htmlTreeParse(paste0(
    home,
    "/Proyectos/FloraArgentina/Especies.asp?Letra=", letter
  ),
  useInternalNodes = TRUE, encoding = "UTF-8"
  )
  Link <- xpathSApply(Link, "//a/@href")
  Link <- Link[grepl("forma=", Link)]
  Link <- paste0(home, Link)
  return(Link)
}

#' @name conosur_species
#'
#' @title Main species list from the Flora del Conosur
#'
#' @description
#' Retrieve species list from the alphabetic overview in
#' [Flora del Conosur](http://www.darwin.edu.ar/Proyectos/FloraArgentina/Especies.asp).
#' The species list is organized in the page by initials of the genera and can
#' be downloaded letter-wise.
#'
#' @param letter Character vector indicating the initials of requested names.
#' @param progress Logical value indicating whether a progress bar should be
#'     initiated or not. The progress bar will be called by [tkProgressBar()].
#'     If length of argument `'letter'` is 1, no progress bar will be called.
#'
#' @return
#' An object of class [cs_species-class], which is in fact a data frame
#' containing following variables:
#' \describe{
#'   \item{TaxonUsageID}{ID of the species names in Flora del Conosur.}
#'   \item{TaxonConceptID}{ID of the taxon concept in Flora del Conosur
#'     (the same as TaxonUsageID).}
#'   \item{TaxonName}{Full name of the taxon (excluding author name).}
#'   \item{genus}{Genus name.}
#'   \item{species}{Species name.}
#'   \item{subspecies}{Subspecies name for subspecies.}
#'   \item{variety}{Variety name for varieties.}
#'   \item{form}{Form name for forms.}
#'   \item{url}{Link to the specific taxon page.}
#' }
#'
#' @author Miguel Alvarez, \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## List for letter A
#' splist <- conosur_species(letter = "Z")
#' head(splist)
#' tail(splist)
#'
#' ## This function is also working with words
#' splist <- conosur_species(letter = "kilo")
#' head(splist)
#' tail(splist)
#'
#' @export conosur_species
conosur_species <- function(letter = LETTERS, progress = TRUE) {
  # internal objects
  letter <- toupper(substr(letter, 1, 1))
  home <- "http://www.darwin.edu.ar"
  ## t_ranks <- c("genero", "especie", "subespecie", "variedad", "forma")
  query <- list()
  if (length(letter) == 1) {
    progress <- FALSE
  }
  if (progress) {
    pb <- tkProgressBar(
      min = 0, max = length(letter), width = 300,
      title = "Loading alphabetic list"
    )
    for (i in letter) {
      query[[i]] <- {
        idx <- which(letter == i)
        Sys.sleep(0.1)
        setTkProgressBar(pb, idx,
          label = paste0(
            i, " (",
            round(idx / length(letter) * 100, ),
            "% done)"
          )
        )
        get_url(home, i)
      }
    }
    close(pb)
  } else {
    for (i in letter) query[[i]] <- get_url(home, i)
  }
  # To data frame
  query <- do.call(c, query)
  OUT <- list()
  OUT$TaxonConceptID <- OUT$TaxonUsageID <-
    as.integer(sub(".*espcod=(.+)$", "\\1", query))
  for (i in c("genero", "especie", "subespecie", "variedad", "forma")) {
    OUT[[i]] <- sub(paste0(".*", i, "=(.+?)&.*"), "\\1", query)
    OUT[[i]][grepl("&", OUT[[i]], fixed = TRUE)] <- ""
  }
  # Build taxon name
  OUT$TaxonName <- paste(OUT$genero, OUT$especie)
  OUT$Level <- rep("species", length(OUT$TaxonName))
  idx <- OUT$subespecie != ""
  OUT$TaxonName[idx] <- paste(OUT$TaxonName[idx], "ssp.", OUT$subespecie[idx])
  OUT$Level[idx] <- "subspecies"
  idx <- OUT$variedad != ""
  OUT$TaxonName[idx] <- paste(OUT$TaxonName[idx], "var.", OUT$variedad[idx])
  OUT$Level[idx] <- "variety"
  idx <- OUT$forma != ""
  OUT$TaxonName[idx] <- paste(OUT$TaxonName[idx], "f.", OUT$forma[idx])
  OUT$Level[idx] <- "form"
  OUT$Level <- factor(OUT$Level, levels = c(
    "form", "variety", "subspecies",
    "species"
  ))
  OUT$url <- query
  OUT <- as.data.frame(OUT, stringsAsFactors = FALSE)
  names(OUT) <- replace_x(names(OUT),
    old = c("genero", "especie", "subespecie", "variedad", "forma"),
    new = c("genus", "species", "subspecies", "variety", "form")
  )
  class(OUT) <- c("cs_species", "data.frame")
  return(OUT)
}
