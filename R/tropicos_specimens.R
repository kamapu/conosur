# TODO:   Query to specimens in Tropicos
# 
# Author: Miguel Alvarez
################################################################################

# browseURL("http://www.tropicos.org/LinkHelp.aspx")

tropicos_specimens <- function(Home="http://www.tropicos.org/SpecimenSearch.aspx",
		collector, number, barcode, accession, specimen, mode="collector",
		encoding="UTF-8") {
	mode <- match.arg(tolower(mode), c("collector","barcode","accession",
					"specimen"))
	if(mode == "collector") {
		if(length(collector) == 1 & length(number) > 1)
			rep(collector, length(number))
		Home <- paste0(Home, "?sen=", collector, "&num=", number)
	}
	if(mode == "barcode")
		Home <- paste0(Home, "?barcode=", barcode)
	if(mode == "accession")
		Home <- paste0(Home, "?accession=", accession)
	if(mode == "specimen")
		Home <- paste0(sub("Search.aspx", "", Home), "/", specimen)
	OUT <- list()
	pb <- tkProgressBar(min=0, max=length(Home), width=300)
	for(i in 1:length(Home)) {
		Sys.sleep(0.1)
		setTkProgressBar(pb, i, title=paste0("Entry ", i, " of ", length(Home),
						" (", round(i/length(Home)*100), "% done)"))
		Page <- read_html(Home[i])
		Name <- html_nodes(Page,
				"#ctl00_MainContentPlaceHolder_specimenDetailsTabControl_FullNameLink")
		OUT[[i]] <- c(html_text(Name), Home[i])
	}
	close(pb)
	names(OUT) <- NULL
	OUT <- as.data.frame(do.call(rbind, OUT), stringsAsFactors=FALSE)
	colnames(OUT) <- c("TaxonName", "Query")
	OUT[with(OUT, TaxonName == Query), "TaxonName"] <- NA
	return(OUT)
}
