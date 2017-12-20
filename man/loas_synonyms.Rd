\name{load_synonyms}
\alias{load_synonyms}

\title{Get Synonyms for conosur_species Output.}
\description{
Get synonyms from species lists loaded by \code{\link{conosur_species}}.
}
\usage{
load_synonyms(x, Home="http://www.darwin.edu.ar", progress=TRUE)
}
\arguments{
\item{x}{Data frame as output of \code{\link{conosur_species}}.}
\item{Home}{Character value indicating the database address.}
\item{progress}{Logical value indicating whether a progress bar should be
	displayed (the default) or not.}
}
\details{
\code{'load_synonyms'} accesses to the specific home page of single species
(indicated in the column \code{'Link'} of the \code{\link{conosur_species}} output).
}
\value{
A list containing synonyms in data frames, each data frame corresponding to a
species in \code{'x'}.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(conosur)

## List for letter A
splist <- conosur_species(Letter="A", collapse=TRUE)
synonyms <- load_synonyms(splist[1:5,])

## Collapse to a single data frame and display the head
synonyms <- do.call(rbind, synonyms)
head(synonyms)
}
