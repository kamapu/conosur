\name{conosur_query}
\alias{conosur_query}

\title{Query for species in \dQuote{Flora del Conosur}.}
\description{
Retrieve data related to species or genera.
}
\usage{
conosur_query(genus, species, ...)
}
\arguments{
\item{genus}{Character vector with names or partial names of requested genera.}
\item{species}{Character vector with names or partial names of species in the
	requested genera.}
\item{...}{Further arguments passed to internal functions (see details).}
}
\details{
This function retrieves information about species contained in
\dQuote{Flora del Conosur}, matching the arguments \code{'genus'} and
\code{'species'}.

This function uses internally \code{\link{conosur_species}} and a modified version of
\code{\link{load_author}} and \code{\link{load_synonyms}}. With
\code{'...'} you can pass values for arguments in the mentioned functions
(i.e. arguments \code{'Home'} and \code{'progress'}).
}
\value{
A list with two elements:
\describe{
	\item{SpList:}{A data frame provided as output from
		\code{\link{conosur_species}}, including author names.}
	\item{Synonyms:}{A list provided as output from
		\code{\link{load_synonyms}}.}
}
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(conosur)

## Query for Weinmannia trichosperma
species <- conosur_query("Weinmannia", "trichosperma")
species

## Partial matchings allowed
species <- conosur_query("Weinm", "trich")
species

## The function is not case-sensitive
species <- conosur_query("weinm", "TRICH")
species

## Query for a genus
species <- conosur_query("Weinmannia")
species
}
