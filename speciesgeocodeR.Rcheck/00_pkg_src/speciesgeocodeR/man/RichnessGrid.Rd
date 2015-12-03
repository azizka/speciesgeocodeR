\name{RichnessGrid}
\alias{RichnessGrid}
\title{
Species Richness and Abundance Grids from Point Records
}
\description{
Create a species richness or abundance grid, from a spgeoOUT object or a data.frame with species names and occurrence coordinates.
}
\usage{
RichnessGrid(x, limits = c(-180, 180, -90, 90), reso,
             type = c("spnum", "abu"))
}

\arguments{
  \item{x}{
an object of the class \code{data.frame} or \code{spgeoIN} or \code{spgeoOUT} or \code{character string}.  If x is a \code{data.frame}, it must consist of three columns, indicating species name, longitude and latitude.  If x is a character string, it can be the name of a tab delimited text file, with three columns indicating species name, longitude and latitude or a vector of species names.  In the latter case the GBIF database is search for geo-referenced occurrences.
}
  \item{limits}{
numerical.  A vector of the form (xmin, xmax, ymin, ymax) givin the raster limits in decimal degrees.
}
  \item{reso}{
numeric.  resolution of the grid in minutes.
}
  \item{type}{
character string indicating the type of data displayed: \dQuote{spnum} = number of species per grid cell, \dQuote{abu} = the number of occurrence points per grid cell.
}
}
\value{
a \code{\link{raster}} object.
}
\seealso{
 \code{\link{MapRichness}}, \code{\link{MapGrid}}, \code{\link{RangeRichness}}.
}
\examples{
# for x = data.frame
data(lemurs)
e <- c(-125, -105, 30, 50)
dat <- RichnessGrid(lemurs, e, reso = 60, type = "spnum")

# for x = character string
e <- c(-125, -105, 30, 50)

\dontrun{
dat <- RichnessGrid("Indri indri", e, reso = 60, type = "spnum")
}
}
\keyword{ spatial }