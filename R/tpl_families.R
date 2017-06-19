# TODO:   Load list of genera and families from The Plant List
# 
# Author: Miguel Alvarez
################################################################################

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
