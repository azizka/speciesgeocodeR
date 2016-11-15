\name{summary.spgeoOUT}
\alias{summary.spgeoOUT}

\title{
Summary Method for SpgeoOUT
}
\description{
summary method for objects of the class \code{spgeoOUT}
}
\usage{
\method{summary}{spgeoOUT}(object, ...)
}
\arguments{
  \item{object}{
an object of the class spgeoOUT
}
  \item{\dots}{
arguments to be passed to methods.
}
}
\details{
The summary gives an overview of the number of species, occurrence points and number of polygons in the object, and gives a summary of the input coordinates as well as the polygon names.  Additionally it shows a summary of species diversity per polygon and the number of species not classified to any polygon.
}
\examples{
data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
summary(outp)
}

\keyword{ methods }