# TODO:   Add Author Names to conosur Output
# 
# Author: Miguel Alvarez
################################################################################

load_author <- function(x, progress=TRUE) {
    if(!"splist" %in% class(x))
        stop("'x' have to be an output of 'conosur_species'", call.=FALSE)
    if(class(x)[1] == "list") {
        x <- do.call(rbind, x)
        rownames(x) <- x$TaxonUsageID
        class(x) <- c(class(x), "splist")
    }
	x$AuthorName <- ""
	if(progress == TRUE & nrow(x) > 1) {
        pb <- tkProgressBar(min=0, max=nrow(x), width=300)
    }
	for(i in rownames(x)) {
		if(progress == TRUE & nrow(x) > 1) {
			Sys.sleep(0.1)
			setTkProgressBar(pb, which(rownames(x) == i),
					title=paste("Completing entries:",
							round(which(rownames(x) == i)/nrow(x)*100,),
							"% done"))
		}
		Page <- htmlTreeParse(x[i,"Link"], useInternalNodes=TRUE,
				encoding="UTF-8")
		Table <- readHTMLTable(Page, stringsAsFactors=FALSE)[[1]]
		Table <- Table[!is.na(Table[,1]),]
		Index <- max(which(nchar(x[i,c("species","subspecies","variety",
												"form")]) > 1))
		# Subspecies identic to species (sensu stricto) ------------------------
		if(Index > 1 & x[i,"species"] == x[i,c("species","subspecies","variety",
						"form")[Index]]) Index <- 1
		# ----------------------------------------------------------------------
		x[i,"AuthorName"] <- Table[Table[,1] == c("Sigla sp.",
						"Sigla ssp.","Sigla var.","Sigla f.")[Index],2]
	}
	if(progress == TRUE & nrow(x) > 1) close(pb)
    x$AuthorName <- iconv(x$AuthorName, "UTF-8", "UTF-8")
	x <- x[,c("TaxonUsageID","TaxonConceptID","TaxonName","AuthorName","genus",
					"species","subspecies","variety","form","Link")]
	return(x)
}
