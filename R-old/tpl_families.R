# TODO:   Load list of genera and families from The Plant List
# 
# Author: Miguel Alvarez
################################################################################



#' Retrieve List of Genera and Families from \dQuote{The Plant List}.
#' 
#' Retrieve a table of families and genera as displayed in the page of
#' \dQuote{The Plant List} (\url{http://www.theplantlist.org/1.1/browse/-/-/}).
#' 
#' This function is adapted to the current structure of the home page and may
#' not require changes in any argument values.
#' 
#' List of genera may contain duplicated entries and for further relations with
#' species lists links to accepted genera must be preferred.
#' 
#' Genera names in The Plant List are written without accents.
#' 
#' @param Home Character value indicating the Internet address of the database.
#' @return A data frame containing following variables: \describe{
#' \item{genus:}{Genus name.} \item{family:}{Name of the respective family.}
#' \item{status:}{Status of the genus (accepted or unresolved).} }
#' @author Miguel Alvarez, \email{malvarez@@uni-bonn.de}
#' @seealso \code{\link{conosur_families}}
#' @examples
#' 
#' library(conosur)
#' 
#' ## Standard use
#' families <- tpl_families()
#' head(families)
#' tail(families)
#' 
#' @export tpl_families
tpl_families <- function(Home="http://www.theplantlist.org/1.1/browse/-/-/") {
    Page <- htmlTreeParse(Home, useInternalNodes=TRUE)
    Families <- unlist(xpathApply(Page, "//i[@class='family']", xmlValue))
    Genera <- getNodeSet(Page, "//li/a/i")
    Accepted <- unlist(lapply(Genera, function(x) grepl("Accepted",
                                xmlAttrs(x))))
    Genera <- unlist(lapply(Genera, xmlValue))
    Table <- data.frame(genus=Genera, family=Families, status="unresolved",
            stringsAsFactors=FALSE)
    Table[Accepted,"status"] <- "accepted"
    return(Table)
}
