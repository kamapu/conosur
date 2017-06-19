\name{load_info}
\alias{load_info}

\title{
Get Information from the Tabular Display of Plant Species in
\dQuote{Flora del Conosur}.
}
\description{
Add information accessing to the specific pages of species loaded by
\code{\link{conosur_species}}.
}
\usage{
load_info(x, entries, labels, progress=TRUE)
}
\arguments{
	\item{x}{Data frame as output of \code{\link{conosur_species}}.}
	\item{entries}{Character vector indicating the name of the field to be
		loaded.}
	\item{labels}{Character vector with alternative names for the information in
		the output table.}
	\item{progress}{Logical value indicating whether a progress bar should be
		displayed (the default) or not.}
}
\details{
\code{'load_info'} accesses to the specific homepage of single species
(indicated in the column \code{'Link'} of the \code{\link{conosur_species}} output).
Retrieved information will be added to the input table (\code{'x'}) as new
columns with the names specified in \code{'labels'}. If \code{'labels'} are not
indicated, the names in \code{'entries'} will be used.

To see the fields available, go to the homepage of \dQuote{Flora del Conosur}
(\url{http://www2.darwin.edu.ar/Proyectos/FloraArgentina/fa.htm}) and search for
or browse to one species.
The tabular information displayed in the page entitled \dQuote{Detalle de la
Especie} indicates available fields.
}
\value{
Data frame \code{'x'} including additional columns with loaded information.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(conosur)
splist <- conosur_species(Letter="A", collapse=TRUE)

## Add the family and status to the species lists
splist <- load_info(splist[1:7,], entries=c("Familia","Status"), labels=c("family","status"))
splist
}
