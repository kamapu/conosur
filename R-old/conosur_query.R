# TODO:   Species query for Flora del Conosur
# 
# Author: Miguel Alvarez
################################################################################



#' Query for species in \dQuote{Flora del Conosur}.
#' 
#' Retrieve data related to species or genera.
#' 
#' This function retrieves information about species contained in \dQuote{Flora
#' del Conosur}, matching the arguments \code{'genus'} and \code{'species'}.
#' 
#' This function uses internally \code{\link{conosur_species}} and a modified
#' version of \code{\link{load_author}} and \code{\link{load_synonyms}}. With
#' \code{'...'} you can pass values for arguments in the mentioned functions
#' (i.e. arguments \code{'Home'} and \code{'progress'}).
#' 
#' @param genus Character vector with names or partial names of requested
#' genera.
#' @param species Character vector with names or partial names of species in
#' the requested genera.
#' @param ... Further arguments passed to internal functions (see details).
#' @return A list with two elements: \describe{ \item{SpList:}{A data frame
#' provided as output from \code{\link{conosur_species}}, including author
#' names.} \item{Synonyms:}{A list provided as output from
#' \code{\link{load_synonyms}}.} }
#' @author Miguel Alvarez, \email{malvarez@@uni-bonn.de}
#' @examples
#' 
#' library(conosur)
#' 
#' ## Query for Weinmannia trichosperma
#' species <- conosur_query("Weinmannia", "trichosperma")
#' species
#' 
#' ## Partial matchings allowed
#' species <- conosur_query("Weinm", "trich")
#' species
#' 
#' ## The function is not case-sensitive
#' species <- conosur_query("weinm", "TRICH")
#' species
#' 
#' ## Query for a genus
#' species <- conosur_query("Weinmannia")
#' species
#' 
#' @export conosur_query
conosur_query <- function(genus, species, ...) {
	if(missing(genus)) stop("genus is mandatory", call.=FALSE)
	message("Step 1 of 2: loading main list")
	Letter <- unique(toupper(substr(genus, 1, 1)))
	Table <- conosur_species(Letter, collapse = TRUE, ...)
	# Matching on genus
	matchGenus <- list()
	for(i in unique(genus)) matchGenus[[i]] <- grepl(i, Table$genus,
				ignore.case=TRUE)
	Table <- Table[apply(as.data.frame(matchGenus), 1, any),]
	# Matching on species
	if(!missing(species)) {
		matchSp <- list()
		for(i in unique(species)) matchSp[[i]] <- grepl(i, Table$species,
					ignore.case=TRUE)
		Table <- Table[apply(as.data.frame(matchSp), 1, any),]
	}
	cat("Step 2 of 2: getting authors and synonyms", "\n")
	Data <- CSgetBoth(Table, ...)
	Table$AuthorName <- Data$Authors
	Table <- Table[,c("TaxonUsageID","TaxonConceptID","TaxonName","AuthorName",
					"genus","species","subspecies","variety","form","Link")]
	return(list(SpList=Table, Synonyms=Data$Synonyms))
}
