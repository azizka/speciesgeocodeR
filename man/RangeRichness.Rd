\name{RangeRichness}
\alias{RangeRichness}

\title{
Species Richness Raster from Range Polygons
}
\description{
Calculate species richness for the specified grid based on an list of range polygons, as derived from \code{\link{CalcRange}}.
}
\usage{
RangeRichness(ra, limits = c(-180, 180, -90, 90), reso = 1, terrestrial = FALSE)
}

\arguments{
  \item{ra}{
A list of SpatialPolygons with species ranges.
}
  \item{limits}{
numeric.  A vector with the limits of the grid in decimal degrees of the format c(xmin, xmax, ymin, ymax). Default is worldwide c(-180, 180, -90, 90).
}
  \item{reso}{
numeric.  Resolution of the grid in minutes.
}
  \item{terrestrial}{
logical.  If TRUE, oceans are omitted.
}
}

\value{
a \code{\link{raster}} object.
}

\examples{
#simulate example occurrences
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

rang <- CalcRange(lemurs, value = "shape")
sprich <- RangeRichness(rang)
MapGrid(sprich)
}
\keyword{spatial}
