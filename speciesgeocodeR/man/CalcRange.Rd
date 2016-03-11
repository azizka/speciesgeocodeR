\name{CalcRange}
\alias{CalcRange}

\title{
Range polygons, AOO and EOO from occurrence points
}

\description{
Calculate the Area of Occupancy based on a user-defined grid as well as range polygons and the Extent of Occurrence based on the \code{areaPolygon} function of the \code{geosphere}, which can be used for criterion B of the IUCN red list classification using \code{\link{IUCNest}}.
}

\usage{
CalcRange(x, index = c("AOO", "EOO"), eoo.value = c("area", "shape"), 
eoo.terrestrial = TRUE, aoo.gridsize = 4, aoo.reps = 3, 
aoo.xmin = NULL, aoo.xmax = NULL, aoo.ymin = NULL, 
aoo.ymax = NULL, verbose = FALSE)
}

\arguments{
  \item{x}{data.frame of species occurrence records, with 3 columns: 1 = species name, 2 = longitude coordinates, y = latitude coordinates.}
  \item{index}{character string, setting the mode of calculation. Default is both.}
  \item{eoo.value}{character string, setting the output value.  If value = \dQuote{area}: A data.frame with the EOO for each species in square kilometers; if value = \dQuote{shape}, a list of convex hull polygons.}
  \item{eoo.terrestrial}{logial.  If TRUE, the shapes are clipped to continent borders.  Only relevant, if EOO is calculated.}
  \item{aoo.gridsize}{
The area of the grid cells used for the AOO calculation in square kilometers. Folloeing the IUCN recommendations the default is to 4.
}
  \item{aoo.reps}{
The number of replicates for the AOO calculation. See details.  Default = 3.
}
  \item{aoo.xmin}{
The minimum longitude for the AOO grid in degrees. If not specified it will be estimated from the minimum in the data.
}
  \item{aoo.xmax}{
The maximum longitude for the AOO grid in degrees. If not specified it will be estimated from the minimum in the data.
}
  \item{aoo.ymin}{
The minimum latitude for the AOO grid in degrees. If not specified it will be estimated from the minimum in the data.
}
  \item{aoo.ymax}{
The minimum latitude for the AOO grid in degrees. If not specified it will be estimated from the minimum in the data.
}
  \item{verbose}{If TRUE, status will be reported.}
}
\details{
As the starting point of the AOO grid can influence the result (depending on ehere the boarders of the gridcells fall, occurrence points might or might not be in the same cell by chance), a replication with different grids is recommended. The starting position of each grid is determined by aoo.xmin and aoo.ymin plus (sqrt(aoo.gridsize) * 1000) / aoo.reps. SO the higher the number of replicates the smaller the difference between the replicates will be.
}

\value{If value is not \dQuote{shape} an object of the class \dQuote{range.sizes} with a data.frame of AOO or EOO values (or both) in square kilometers, if value = \dQuote{shape}, a list of convex hull polygons.}

\note{
See the speciesgeocodeR documentation for further information and examples.
}

\examples{
data(lemurs)
CalcRange(lemurs, index = c("AOO", "EOO"), eoo.value = "area", eoo.terrestrial = F
\dontrun{
CalcRange(lemurs, index = "EOO", eoo.value = "shape", eoo.terrestrial = F)
CalcRange(lemurs, index = "EOO", eoo.value = "area", eoo.terrestrial = F)
CalcRange(lemurs, index = "AOO", eoo.value = "area")
)
}
}
\keyword{ spatial }