# TODO:   Add tabular infos to names (replacement of CSgetAuthor)
# 
# Author: Miguel Alvarez
################################################################################



#' Get Information from the Tabular Display of Plant Species in \dQuote{Flora
#' del Conosur}.
#' 
#' Add information accessing to the specific pages of species loaded by
#' \code{\link{conosur_species}}.
#' 
#' \code{'load_info'} accesses to the specific homepage of single species
#' (indicated in the column \code{'Link'} of the \code{\link{conosur_species}}
#' output). Retrieved information will be added to the input table (\code{'x'})
#' as new columns with the names specified in \code{'labels'}. If
#' \code{'labels'} are not indicated, the names in \code{'entries'} will be
#' used.
#' 
#' To see the fields available, go to the homepage of \dQuote{Flora del
#' Conosur} (\url{http://www.darwin.edu.ar/Proyectos/FloraArgentina/fa.htm})
#' and search for or browse to one species. The tabular information displayed
#' in the page entitled \dQuote{Detalle de la Especie} indicates available
#' fields.
#' 
#' @param x Data frame as output of \code{\link{conosur_species}}.
#' @param entries Character vector indicating the name of the field to be
#' loaded.
#' @param labels Character vector with alternative names for the information in
#' the output table.
#' @param progress Logical value indicating whether a progress bar should be
#' displayed (the default) or not.
#' @return A list containing the requested information.
#' @author Miguel Alvarez, \email{kamapu78@@gmail.com}
#' @examples
#' 
#' library(conosur)
#' splist <- conosur_species(Letter="A", collapse=TRUE)
#' 
#' ## Add the family and status to the species lists
#' splist <- load_info(splist[1:7,], entries=c("Familia","Status"), labels=c("family","status"))
#' splist
#' 
#' @export load_info
load_info <- function(x, entries, labels, progress=TRUE) {
    if(!"splist" %in% class(x))
        stop("'x' have to be an output of 'conosur_species'", call.=FALSE)
    if(class(x)[1] == "list") {
        x <- do.call(rbind, x)
        rownames(x) <- x$TaxonUsageID
        class(x) <- c(class(x), "splist")
    }
    if(missing(labels)) labels <- entries
	Info <- list()
	if(progress) pb <- tkProgressBar(min=0, max=nrow(x), width=300)
	for(i in rownames(x)) {
        tryCatch({if(progress) {
                        Sys.sleep(0.1)
                        setTkProgressBar(pb, which(rownames(x) == i),
                                title=paste("Completing entries:",
                                        round(which(rownames(x) == i)/
                                                        nrow(x)*100,),
                                        "% done"))
                    }
                    Page <- htmlTreeParse(x[i,"Link"], useInternalNodes=TRUE,
                            encoding="UTF-8")
                    Table <- readHTMLTable(Page, stringsAsFactors=FALSE)[[3]]
                    Table[!is.na(Table[,2]) & Table[,2] == "",2] <- NA
                    Table[!is.na(Table[,2]) & Table[,2] == "-",2] <- NA
                    ## for(j in 1:ncol(Table)) {
                    ##     Table[,j] <- iconv(Table[,j], to="LATIN1")
                    ## }
                    Info[[i]] <- Table[match(iconv(entries,
                                            to="ASCII//TRANSLIT"),
                                    iconv(Table[,1], to="ASCII//TRANSLIT")) ,2]
                    names(Info[[i]]) <- labels
                }, error=function(e) {
                    cat("ERROR :", conditionMessage(e), "\n")
                }
        )
    }
	if(progress) close(pb)
    ## Info <- do.call(rbind, Info)
    ## for(i in colnames(Info)) x[,i] <- iconv(Info[,i], to="LATIN1")
    ## return(x)
    return(Info)
}


