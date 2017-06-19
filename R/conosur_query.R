# TODO:   Species query for Flora del Conosur
# 
# Author: Miguel Alvarez
################################################################################

conosur_query <- function(genus, species, ...) {
	if(missing(genus)) stop("genus is mandatory", call.=FALSE)
	cat("Step 1 of 2: loading main list", "\n")
	Letter <- unique(toupper(substr(genus, 1, 1)))
	Table <- conosur_species(Letter, collapse=TRUE, ...)
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
