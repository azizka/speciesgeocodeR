\name{ReadPoints}
\alias{ReadPoints}

\title{
Loading Data into SpeciesgeocodeR
}
\description{
Create an object of the class spgeoIN from the input arguments.
}
\usage{
ReadPoints(x, y, areanames = NA, verbose = FALSE, cleaning = FALSE)
}

\arguments{
  \item{x}{
a \code{data.frame} with three columns named identifier, XCOOR, YCOOR or a \code{character string}.  If x is a \code{character string}, it can either be the path to a tab-delimited text file containing the coordinates of points of interest (tab delimited, three columns with header: identifier, XCOOR, YCOOR) OR a vector of species names.   In the latter cases GBIF is searched for occurrence data using the rgbif function of the rgbif package. 
}
  \item{y}{
character string giving the path to a text file containing the polygons of points of interest (tab delimited, three columns with header: identifier, XCOOR, YCOOR) OR an object of the class \code{\link{SpatialPolygons}} OR an object of the class
 \code{\link{SpatialPolygonsDataFrame}}.
}
  \item{areanames}{
character string.  If the polygons in the spgeoIN object were derived from a shape file or a SpatialPolygonsDataframe, this column indicates the name of the column used for area names.   See the Speciesgeocoder tutorial for more information.  If the polygons in the spgeoIN object were derived from a text file leave as default (Default = NA).
}
  \item{verbose}{
logical. If TRUE, the functions report the progress of the analyses. This is advisable for large datasets.
}
  \item{cleaning}{
logical. If TRUE, \code{GeoClean} is applied to the input coordinates. Default = FALSE.
}
  \item{\dots}{
Further arguments to be passed to \code{GeoClean}.
}
}
\value{
An object of class spgeoIN.
}
\note{
See the speciesgeocodeR tutorial for further information and examples.
}
\seealso{
 \code{\link{lemurs_in}}
}
\examples{
data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
}

\keyword{ IO }