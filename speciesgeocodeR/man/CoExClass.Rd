\name{CoExClass}
\alias{CoExClass}
\title{
Species Coexistence Matrices in Given Areas.
}
\description{
Add a coexistence matrix to an object of the class spgeoOUT created by \code{\link{SpGeoCod}}.
}
\usage{
CoExClass(x, verbose = F)
}
\arguments{
  \item{x}{An object of the class spgeoOUT, where the coexistence matrix shall be added}
  \item{verbose}{If TRUE, status will be reported.}
}

\value{
An object of the class spgeoOUT, including a coexistence matrix which can be addressed with via the \dQuote{coexistence_classified} slot.  The matrix will have the same dimensions as there are input species and will give the percent of occurrence points of each species (row) with all other species (columns) given the input polygons.
}
\note{
See the speciesgeocodeR tutorial for further information and examples.
}
\examples{
data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
outpcoex <- CoExClass(outp)
outpcoex$coexistence_classified
}
\keyword{ spatial }
