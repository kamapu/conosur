\name{tpl_families}
\alias{tpl_families}

\title{
Retrieve List of Genera and Families from \dQuote{The Plant List}.
}
\description{
Retrieve a table of families and genera as displayed in the page of
\dQuote{The Plant List}
(\url{http://www.theplantlist.org/1.1/browse/-/-/}).
}
\usage{
tpl_families(Home="http://www.theplantlist.org/1.1/browse/-/-/")
}
\arguments{
\item{Home}{Character value indicating the Internet address of the database.}
}
\details{
This function is adapted to the current structure of the home page and may not
require changes in any argument values.

List of genera may contain duplicated entries and for further relations with
species lists links to accepted genera must be preferred.

Genera names in The Plant List are written without accents.
}
\value{
A data frame containing following variables:
\describe{
	\item{genus:}{Genus name.}
	\item{family:}{Name of the respective family.}
	\item{status:}{Status of the genus (accepted or unresolved).}
}
}
\seealso{
\code{\link{conosur_families}}
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(conosur)

## Standard use
families <- tpl_families()
head(families)
tail(families)
}
