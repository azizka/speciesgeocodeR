\name{SpGeoCodH}
\alias{SpGeoCodH}

\title{
A Standard a SpeciesgeocodeR Area Classification
}
\description{
Run a standard speciesgeocodeR occurrence point to area classification analysis on an object of the class spgeoIN (created by \code{\link{ReadPoints}}).  The results are stored in an object of the class spgeoOUT.  See details for more information.
}
\usage{
SpGeoCodH(x, areanames = NULL, occ.thresh = 0)
}

\arguments{
  \item{x}{
an object of the class \code{spgeoIN}
}
  \item{areanames}{
a \code{character string}.  If the polygons in the spgeoIN object were derived from a shape file, this column indicates the name of the column used for area names.   See the Speciesgeocoder tutorial for more information.  If the polygons in the spgeoIN object were derived from a text file leave as default (Default = NA).
}
  \item{occ.thresh}{
numerical. The threshold for the minimum number of occurrences necessary in an area to be counted as present (in percent of the total occurrences of the species).  Default = 0.
}
}
\details{
This function uses an object of the class spgeoIN and performs a point in polygon test classifying each species to a polygon, summarizes the information per samples that could not be classified and calculates a coexistence matrix.  These objects are then put together with the input information to an object of the class \code{spgeoOUT}.
}
\value{
An object of the class spgeoOUT.
}

\note{
See the speciesgeocodeR tutorial for further information and examples.
}

\examples{
data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
names(outp)
}
\keyword{ spatial }