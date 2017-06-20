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


