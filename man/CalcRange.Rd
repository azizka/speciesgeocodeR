\name{CalcRange}
\alias{CalcRange}

\title{
Range polygons from occurrence points
}

\description{
Calculate range polygons based on convex hulls from occurrence points. Includes planar and a \sQuote{pseudospherical} method for lat/long geographic coordinates and can crop to terrestrial area.
}

\usage{
CalcRange(x, method, terrestrial = F)
}

\arguments{
  \item{x}{data.frame of species occurrence records, including columns named \sQuote{species}, \sQuote{decimallongitude}, \sQuote{decimallatitude} OR and object of the class spgeoOUT OR an object of the class SpatialPointDataFrame, with at least one data column named \sQuote{species}.}
  \item{method}{character string, one of \sQuote{euclidean} and \sQuote{pseudospherical} setting the method to calculate convex hull. See details.}
  \item{terrestrial}{logical. If TRUE the polygons are restriceted to terrestrial area as in \code{\link{landmass}}}
}
\details{
Be careful with the polygons when using lat/long data, convex hulls on a spehere are not easily defined. The function is limited to occurrence records on one hemisphere, spanning less than 180 degrees longitude and not crossing the 180/0 boundary!

If method == \sQuote{euclidean}, planar (projected, or local scale) coordinates are expected. If method == \sQuote{pseudospherical} the \code{geosphere::makePoly} is used to define the convex hull. In the latter case, to create the polygon intermediate points are added on a sphere between two coordinates.

}

\value{An object of the class SpatialPolygonsDataFrame.}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}

\examples{
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

CalcRange(occ.exmpl, method = "pseudospherical", terrestrial = FALSE)
}
\keyword{ spatial }
