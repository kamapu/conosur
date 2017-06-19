\name{load_author}
\alias{load_author}

\title{Add Author Names to a conosur_species Output}
\description{
Add author names accessing to the specific pages of species loaded by
\code{\link{conosur_species}}.
}
\usage{
load_author(x, progress=TRUE)
}
\arguments{
\item{x}{Data frame as output of \code{\link{conosur_species}}.}
\item{progress}{Logical value indicating whether a progress bar should be
	displayed (the default) or not.}
}
\details{
\code{'load_author'} accesses to the specific homepage of single species,
which are indicated in the column \code{'Link'} of the
\code{\link{conosur_species}} output.
}
\value{
The same data frame as \code{'x'} but with an additional column named
\code{'AuthorName'}.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(conosur)

## List for letter A
splist <- conosur_species(Letter="A", collapse=TRUE)
splist <- load_author(splist[1:7,])
splist
}
