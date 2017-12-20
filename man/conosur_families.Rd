\name{conosur_families}
\alias{conosur_families}

\title{
Retrieve List of Genera and Respective Families from \dQuote{Flora del Conosur}.
}
\description{
Retrieve a table of families and genera as displayed in the page of
\dQuote{Flora del Conosur}
(\url{http://www.darwin.edu.ar/Proyectos/FloraArgentina/Familias.asp}).
}
\usage{
conosur_families(Home="http://www.darwin.edu.ar", Letters=c("","?Letras=1",
	"?Letras=2"))
}
\arguments{
\item{Home}{Character value indicating the Internet address of the database.}
\item{Letters}{Character vector indicating the suffix for main and subsequent
	alphabetic lists.}
}
\details{
This function is adapted to the current structure of the home page and may not
require changes in any argument values.
}
\value{
A data frame containing following variables:
\describe{
	\item{family:}{Name of the respective family.}
	\item{genus:}{Genus name.}
}
}
\seealso{
\code{\link{tpl_families}}
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
library(conosur)

## Standard use
families <- conosur_families()
head(families)
tail(families)
}
