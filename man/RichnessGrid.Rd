\name{RichnessGrid}
\alias{RichnessGrid}
\title{
Species Richness and Abundance Grids from Point Records
}
\description{
A species richness or record abundance grid.
}
\usage{
RichnessGrid(x, ras, reso = 1, type = "spnum")
}

\arguments{
  \item{x}{
an object of the class  \code{spgeoOUT} or a \code{data.frame}.  If x is a \code{data.frame}, it must comprise three columns,: \sQuote{species}, \sQuote{longitude} and \sQuote{latitude}. 
}
  \item{ras}{
an object of the class \code{raster} defining the extent and resolution for the richness grid.  If missing,  estimated from the data.
}
  \item{reso}{
numeric.  resolution of the raster in decimal degree, only specify if ras is missing.
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

}
\keyword{Diversity pattern}