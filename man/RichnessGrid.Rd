\name{RichnessGrid}
\alias{RichnessGrid}
\title{
Species Richness and Abundance Grids from Point Records
}
\description{
Create a species richness or abundance grid, from a spgeoOUT object or a data.frame with species names and occurrence coordinates.
}
\usage{
RichnessGrid(x, ras, reso = 1, type = "spnum")
}

\arguments{
  \item{x}{
an object of the class \code{data.frame} or \code{spgeoIN} or \code{spgeoOUT} or \code{character string}.  If x is a \code{data.frame}, it must consist of three columns, indicating species name, longitude and latitude.  If x is a character string, it can be the name of a tab delimited text file, with three columns indicating species name, longitude and latitude or a vector of species names.  In the latter case the GBIF database is search for geo-referenced occurrences.
}
  \item{ras}{
a raster object defining the extent and resolution for the 
}
  \item{reso}{
numeric.  resolution of the raster in decimal degree, only speciefy if ras is missing.
}
  \item{type}{
character string indicating the type of data displayed: \dQuote{spnum} = number of species per grid cell, \dQuote{abu} = the number of occurrence points per grid cell.
}
}
\value{
a \code{\link{raster}} object.
}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}

\seealso{
 \code{\link{MapRichness}}, \code{\link{RangeRichness}}.
}
\examples{
# for x = data.frame
data(lemurs)
dat <- RichnessGrid(lemurs, reso = 1, type = "spnum")

# for x = character string
\dontrun{
dat <- RichnessGrid(c("Indri indri", "Lemur catta"), reso = 1, type = "spnum")
}
}
\keyword{ spatial }