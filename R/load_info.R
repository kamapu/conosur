# TODO:   Add tabular infos to names (replacement of CSgetAuthor)
# 
# Author: Miguel Alvarez
################################################################################

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
		if(progress) {
			Sys.sleep(0.1)
			setTkProgressBar(pb, which(rownames(x) == i),
					title=paste("Completing entries:",
							round(which(rownames(x) == i)/nrow(x)*100,),
							"% done"))
		}
		Page <- htmlTreeParse(x[i,"Link"], useInternalNodes=TRUE,
				encoding="UTF-8")
		Table <- readHTMLTable(Page, stringsAsFactors=FALSE)[[2]]
		Table[is.na(Table)] <- ""
		Table <- Table[Table[,1] != "",]
        for(j in 1:ncol(Table)) {
            Table[,j] <- iconv(Table[,j], to="LATIN1")
        }
        Info[[i]] <- Table[match(entries, Table[,1]),2]
		names(Info[[i]]) <- labels
	}
	if(progress) close(pb)
	Info <- do.call(rbind, Info)
	for(i in colnames(Info)) x[,i] <- iconv(Info[,i], to="LATIN1")
	return(x)
}
