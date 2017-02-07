\name{RangeRichness}
\alias{RangeRichness}

\title{
Species Richness Raster from Range Polygons
}
\description{
Calculate species richness for the specified grid based on an list of range polygons, as derived from \code{\link{CalcRange}}.
}
\usage{
RangeRichness(x, ras, reso = 1, buffer = 1, terrestrial = FALSE)
}

\arguments{
  \item{x}{
A list of SpatialPolygons with species ranges.
}
  \item{ras}{
Template raster to rasterize x.
}
  \item{reso}{
numeric.  Resolution of the raster in decimal degree.  Only if ras is missing.
}
\item{buffer}{
numerical.  The buffer around the polygon extent when plotting the map.
}
  \item{terrestrial}{
logical.  If TRUE, oceans are omitted.
}
}

\value{
a \code{\link{raster}} object.
}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}

\examples{
#simulate example occurrences
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

rang <- CalcRange(occ.exmpl, method = 'pseudospherical')
sprich <- RangeRichness(rang)
plot(sprich)
}
\keyword{spatial}
