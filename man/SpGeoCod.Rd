\name{SpGeoCod}
\alias{SpGeoCod}
\alias{is.spgeoOUT}
\alias{as.data.frame.spgeoOUT}
\alias{summary.spgeoOUT}

\title{
Point to Polygon Classification
}
\description{
Classifying point occurrences to polygons for the use in phylogenetic analysis.
}
\usage{
SpGeoCod(x, y, areanames, occ.thresh = 0, elevation, threshold)
}

\arguments{
  \item{x}{
a \code{data.frame} with three columns named: species, decimallongitude, decimallatitude OR a \code{character string} with the path to a text file containing the coordinates of points of interest (tab delimited, three columns with header: species, decimallongitude, decimallatitude) OR a \code{character string} giving species names, to download from GBIF.
}
  \item{y}{
an  \code{SpatialPolygonsDataframe} OR a \code{character string} with the path to a shape file containing the polygons of interet OR the path to a text file containing the polygons of interest (tab delimited, three columns with header: species, decimallongitude, decimallatitude) OR an object of the class SpatialPolygons .
}
  \item{areanames}{
a \code{character string}.  If \code{y} is a \code{SpatialPolygonsDataFrame} or the path to a shape file, this column indicates the column with the area names. If the polygon input file is a text file leave as default (Default = ""). 
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

\keyword{ Area classification }