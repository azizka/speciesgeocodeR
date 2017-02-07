\name{SpGeoCod}
\alias{SpGeoCod}
\alias{is.spgeoOUT}
\alias{as.data.frame.spgeoOUT}
\alias{summary.spgeoOUT}

\title{
Point to Polygon Classification from Text Files
}
\description{
Runs a complete speciesgeocodeR occurrences to area classification analysis from text files as input.
}
\usage{
SpGeoCod(x, y, areanames, occ.thresh = 0, elevation, threshold, ...)
}

\arguments{
  \item{x}{
a \code{character string} giving the path to a text file containing the coordinates of points of interest (tab delimited, three columns with header: species, decimallongitude, decimallatitude) OR a \code{data.frame} with three columns named: species, decimallongitude, decimallatitude.
}
  \item{y}{
a \code{character string} giving the path to a shape file containing the polygons of interet OR a text file containing the polygons of interest (tab delimited, three columns with header: species, decimallongitude, decimallatitude) OR an object of the class SpatialPolygons OR an object of the class \code{SpatialPolygonsDataframe}.
}
  \item{areanames}{
a \code{character string}.  If the input polygon file is a shape and not a text file, this column indicates the name of the column used for area names.  See the SpeciesgeocodeR tutorial for more information, if the polygon input file is a text file leave as default (Default = ""). 
}
  \item{occ.thresh}{
numerical. Threshold for the minimum number of occurrences necessary in a polygon to be counted as present (in percent of the total occurrences of the species).  Default = 0.
}
 \item{elevation}{
logical.  Should the results be split by elevation class? (default = FALSE)
}
  \item{threshold}{
numeric vector.  Indicating the elevation thresholds.
}
  \item{\dots}{
Further arguments to be passed to \code{GeoClean}.
}
}

\value{
An object of the class spgeoOUT.
}
\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}
\examples{
data(lemurs)
data(mdg_biomes)

outp <- SpGeoCod(lemurs, mdg_biomes, areanames = "name")
}

\keyword{ spatial }