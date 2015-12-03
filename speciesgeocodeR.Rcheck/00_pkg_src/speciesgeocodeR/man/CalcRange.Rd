\name{CalcRange}
\alias{CalcRange}
\title{
Calculate the Extent of Occurrence
}
\description{
Uses the \code{areaPolygon} function of the \code{geosphere} package to calculate the extent of occurrence (EOO) for each species in the list, which can be used for criterion B of the IUCN red list classification.}
\usage{
CalcRange(x, mode =  "EOO", value = c("area", "shape"))}
\arguments{
  \item{x}{data.frame, with 3 columns: 1 = species name, 2 = longitude coordinates, y = latitude coordinates.}
  \item{mode}{character string, setting the mode of calculation, currently only \dQuote{EOO} is supported.}
  \item{value}{character string, setting the output value.  If value = \dQuote{area}: A data.frame with the EOO for each species in square kilometers; if value = \dQuote{shape}, a list of convex hull polygons.}
}

\value{If value = \dQuote{area}: A data.frame with the EOO for each species in square kilometers; if value = \dQuote{shape}, a list of convex hull polygons.}

\note{
See the speciesgeocodeR documentation for further information and examples.
}
\examples{
data(lemurs)
CalcRange(lemurs)
}
\keyword{ spatial }
