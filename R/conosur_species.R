# TODO:   Load main species list from Flora del Conosur
# 
# Author: Miguel Alvarez
################################################################################

conosur_species <- function(Letter=LETTERS, Home="http://www2.darwin.edu.ar",
		collapse=FALSE, progress=TRUE) {
	Letter <- toupper(substr(Letter, 1, 1))
	OUT <- list()
	if(progress == TRUE & length(Letter) > 1) progress <- TRUE else
		progress <- FALSE
	if(progress == TRUE) pb <- tkProgressBar(min=0, max=length(Letter),
				width=300)
	for(i in Letter) OUT[[i]] <- {
			if(progress == TRUE) {
				Sys.sleep(0.1)
				setTkProgressBar(pb, which(Letter == i),
						title=paste("Loading alphabetic list:",
								round(which(Letter == i)/length(Letter)*100,),
								"% done"))
			}
			Page <- htmlTreeParse(paste0(Home,
							"/Proyectos/FloraArgentina/Especies.asp?Letra=", i),
					useInternalNodes=TRUE, encoding="UTF-8")
			Link <- xpathSApply(Page, "//a/@href")
			Link <- Link[grepl("forma=", Link)]
			Link <- paste0(Home, Link)
			Name <- sub("/Proyectos/FloraArgentina/DetalleEspecie.asp?", "",
					Link, fixed=TRUE)
			Name <- gsub("&", "", Name)
			for(j in c("forma=","variedad=","subespecie=","especie=", "genero=",
					"espcod=")) {
				Name <- gsub(j, "\t", Name)
			}
			Name <- strsplit(Name, "\t", fixed=TRUE)
			Name <- do.call(rbind, Name)[,-1]
			colnames(Name) <- c("form","variety","subspecies","species","genus",
					"spcode")
			Fullname <- with(as.data.frame(Name, stringsAsFactors=FALSE), {
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
			Name <- data.frame(Name[,c("spcode","spcode")], Fullname,
					Name[,c("genus","species","subspecies","variety","form")],
					Link, row.names=Name[,"spcode"], stringsAsFactors=FALSE)
			colnames(Name)[1:3] <- c("TaxonUsageID","TaxonConceptID",
                    "TaxonName")
            for(j in colnames(Name)) {
                Name[,j] <- iconv(Name[,j], "UTF-8", "UTF-8")
            }
            Name
		}
	if(progress == TRUE) close(pb)
	if(collapse == TRUE) {
		OUT <- do.call(rbind, OUT)
		rownames(OUT) <- OUT$TaxonUsageID
	}
    class(OUT) <- c(class(OUT), "splist")
	return(OUT)
}
