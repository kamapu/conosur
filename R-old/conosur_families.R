# TODO:   Import list of genera and respective families from Flora del Conosur
# 
# Author: Miguel Alvarez
################################################################################



#' Retrieve List of Genera and Respective Families from \dQuote{Flora del
#' Conosur}.
#' 
#' Retrieve a table of families and genera as displayed in the page of
#' \dQuote{Flora del Conosur}
#' (\url{http://www.darwin.edu.ar/Proyectos/FloraArgentina/Familias.asp}).
#' 
#' This function is adapted to the current structure of the home page and may
#' not require changes in any argument values.
#' 
#' @param Home Character value indicating the Internet address of the database.
#' @param Letters Character vector indicating the suffix for main and
#' subsequent alphabetic lists.
#' @return A data frame containing following variables: \describe{
#' \item{family:}{Name of the respective family.} \item{genus:}{Genus name.} }
#' @author Miguel Alvarez, \email{malvarez@@uni-bonn.de}
#' @seealso \code{\link{tpl_families}}
#' @examples
#' 
#' library(conosur)
#' 
#' ## Standard use
#' families <- conosur_families()
#' head(families)
#' tail(families)
#' 
#' @export conosur_families
conosur_families <- function(Home="http://www.darwin.edu.ar",
		Letters=c("","?Letras=1","?Letras=2")) {
	Table <- list()
	for(i in 1:length(Letters)) {
		Page <- htmlTreeParse(paste0(Home,
						"/Proyectos/FloraArgentina/Familias.asp", Letters[i]),
				useInternalNodes=TRUE, encoding="UTF-8")
		Table[[i]] <- readHTMLTable(Page, stringsAsFactors=FALSE)[[2]]
	}
	Table <- do.call(rbind, Table)
	# Final table
	Families <- strsplit(Table[,2], ",", fixed=TRUE)
	for(i in 1:length(Families)) {
		Families[[i]] <- gsub("^\\s+|\\s+$", "", Families[[i]])
		Families[[i]] <- data.frame(family=Table[i,1], genus=Families[[i]],
				stringsAsFactors=FALSE)
	}
	Families <- do.call(rbind, Families)
    for(i in colnames(Families)) {
        Families[,i] <- iconv(Families[,i], "UTF-8", "UTF-8")
    }
	return(Families[Families$genus != "AAA",])
}
