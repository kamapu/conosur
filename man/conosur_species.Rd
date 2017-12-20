\name{conosur_species}
\alias{conosur_species}

\title{Retrieve Main Species List from \dQuote{Flora del Conosur}.}
\description{
Retrieve species list from the alphabetic overview in \dQuote{Flora del Conosur}
(\url{http://www.darwin.edu.ar/Proyectos/FloraArgentina/Especies.asp}).
}
\usage{
conosur_species(Letter=LETTERS, Home="http://www.darwin.edu.ar",
    collapse=FALSE, progress=TRUE)
}
\arguments{
\item{Letter}{Character vector indicating the initials of requested names.}
\item{Home}{Character value indicating the database address.}
\item{collapse}{Logical value indicating whether the output list should be
	content in a list (the default) or collapsed into a single data frame.}
\item{progress}{Logical value indicating whether a progress bar should be
	displayed (the default) or not.}
}
\details{
\code{conosur_species} retrieves tables of species contained in the main species list
of the database \dQuote{Flora del Conosur}
(\url{http://www.darwin.edu.ar/Proyectos/FloraArgentina/Especies.asp}). This
function also retrieves names for specific initials, which are displayed in
separated pages on the mentioned home page.

The progress bar is implemented by \code{\link{tkProgressBar}} using as
reference of total iterations the length of the argument \code{Letter}. This
progress bar won't be displayed for \code{Letter} of length equal to 1.
}
\value{
For \code{collapse=FALSE} (the default), a list with single data frames for
every requested initial. For \code{collapse=TRUE}, a data frame including all
requested initials.

The respective data frames contain following variables:
\describe{
	\item{TaxonUsageID:}{ID of the species names in Flora del Conosur (also set
		as row names).}
	\item{TaxonConceptID:}{ID of the taxon concept in Flora del Conosur (the
		same as TaxonUsageID).}
	\item{TaxonName:}{Full name of the taxon (excluding author name).}
	\item{genus:}{Genus name.}
	\item{species:}{Species name.}
	\item{subspecies:}{Subspecies name for subspecies.}
	\item{variety:}{Variety name for varieties.}
	\item{form:}{Form name for forms.}
	\item{Link:}{Link to the specific taxon page.}
}
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(conosur)

## List for letter A
splist <- conosur_species(Letter="A", collapse=TRUE)
head(splist)
tail(splist)

## This function is also working with words
splist <- conosur_species(Letter=c("zulu","uniform"))
names(splist)
head(splist[[1]])
head(splist[[2]])
}
