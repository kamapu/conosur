# TODO:   Import list of genera and respective families from Flora del Conosur
# 
# Author: Miguel Alvarez
################################################################################

conosur_families <- function(Home="http://www2.darwin.edu.ar",
		Letters=c("","?Letras=1","?Letras=2")) {
	Table <- list()
	for(i in 1:length(Letters)) {
		Page <- htmlTreeParse(paste0(Home,
						"/Proyectos/FloraArgentina/Familias.asp", Letters[i]),
				useInternalNodes=TRUE, encoding="UTF-8")
		Table[[i]] <- readHTMLTable(Page, stringsAsFactors=FALSE)[[3]]
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
	return(Families)
}
