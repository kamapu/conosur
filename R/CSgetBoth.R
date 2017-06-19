# TODO:   Intern function combining CSfamilies and CSgetSynonyms
# 
# Author: Miguel Alvarez
################################################################################

CSgetBoth <- function(x, Home="http://www2.darwin.edu.ar", progress=TRUE) {
	SpAuthor <- character(0)
	if(progress == TRUE & nrow(x) > 1) pb <- tkProgressBar(min=0, max=nrow(x),
				width=300)
	OUT <- list()
	for(i in rownames(x)) {
		if(progress == TRUE & nrow(x) > 1) {
			Sys.sleep(0.1)
			setTkProgressBar(pb, which(rownames(x) == i),
					title=paste("Getting authors and synonyms:",
							round(which(rownames(x) == i)/nrow(x)*100,),
							"% done"))
		}
		Page <- htmlTreeParse(x[i,"Link"], useInternalNodes=TRUE,
				encoding="UTF-8")
		# Get species author
		MainTable <- readHTMLTable(Page, stringsAsFactors=FALSE)[[2]]
		MainTable <- MainTable[!is.na(MainTable[,1]),]
		Index <- max(which(nchar(x[i,c("species","subspecies","variety",
												"form")]) > 1))
		# Subspecies identic to species (sensu stricto) ------------------------
		if(Index > 1 & x[i,"species"] == x[i,c("species","subspecies","variety",
						"form")[Index]]) Index <- 1
		# ----------------------------------------------------------------------
		SpAuthor[i] <- MainTable[MainTable[,1] == c("Sigla sp.","Sigla ssp.",
						"Sigla var.","Sigla f.")[Index],2]
		# Get Synonyms
		Link <- xpathSApply(Page, "//a/@href")
		Link <- Link[grepl("sinonimoespecie", Link, fixed=TRUE)]
		if(length(Link) == 0) next
		Link2 <- xpathSApply(Page, paste0('//a[@href="', Link, '"]'))
		Link <- paste0(Home, Link)
		# Extracting synonyms
		Names <- character(0)
		for(j in 1:length(Link2)) {
			Names[j] <- xmlValue(Link2[[j]])
		}
		Names <- gsub(",", "", Names, useBytes=TRUE)
		Names <- gsub("^\\s+|\\s+$", "", Names, useBytes=TRUE) # leading or trailing blanks
		Names <- gsub("\\s+", " ", Names, useBytes=TRUE) # double blanks
		# Getting tabular infos
		Table <- gsub("&", "", Link)
		for(j in c("forma=","variedad=","subespecie=","EspCod=","especie=",
				"genero=","SinGeneroDe=","SinEspecieDe=","sincod=")) {
			Table <- gsub(j, "\t", Table)
		}
		Table <- strsplit(Table, "\t", fixed=TRUE)
		Table <- do.call(rbind, Table)[,-1]
		if(is.vector(Table)) Table <- as.data.frame(t(Table),
					stringsAsFactors=FALSE)
		colnames(Table) <- c("form","variety","subspecies","spcode","species",
				"genus","synGenus","synSpecies","TaxonUsageID")
		# Generating full names
		Fullname <- with(as.data.frame(Table, stringsAsFactors=FALSE), {
					form2 <- paste("f.", form)
					form2[form == ""] <- ""
					variety2 <- paste("var.", variety)
					variety2[variety == ""] <- ""
					subspecies2 <- paste("ssp.", subspecies)
					subspecies2[subspecies == ""] <- ""
					Full <- paste(genus, species, subspecies2, variety2,
							form2)
					Full <- gsub("^\\s+|\\s+$", "", Full, useBytes=TRUE) # leading or trailing blanks
					Full <- gsub("\\s+", " ", Full, useBytes=TRUE) # double blanks
					Full
				})
		Author <- character(0)
		for(j in 1:length(Link)) {
			Author[j] <- gsub(Fullname[j], "", Names[j])
		}
		Author <- gsub("^\\s+", "", Author, useBytes=TRUE) # leading blanks
		FinalTable <- list()
		FinalTable[["TaxonConceptID"]] <- rep(i, times=length(Link))
		for(j in c("genus","species","subspecies","variety","form",
				"TaxonUsageID")) {
			FinalTable[[j]] <- Table[,j]
		}
		FinalTable[["TaxonName"]] <- Fullname
		FinalTable[["AuthorName"]] <- Author
		FinalTable[["Link"]] <- Link
		FinalTable <- as.data.frame(FinalTable, stringsAsFactors=FALSE)
        for(j in colnames(FinalTable)) {
            FinalTable[,j] <- iconv(FinalTable[,j], from="UTF-8", to="UTF-8")
        }
        OUT[[i]] <- FinalTable[,c("TaxonUsageID","TaxonConceptID","TaxonName",
						"AuthorName","genus","species","subspecies","variety",
						"form","Link")]
	}
	if(progress == TRUE & nrow(x) > 1) close(pb)
	return(list(Authors=SpAuthor, Synonyms=OUT))
}
