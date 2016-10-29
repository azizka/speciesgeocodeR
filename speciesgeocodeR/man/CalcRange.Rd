\name{CalcRange}
\alias{CalcRange}

\title{
Range polygons, AOO and EOO from occurrence points
}

\description{
Calculate the Area of Occupancy based on a user-defined grid as well as range polygons and the Extent of Occurrence based on an Euclidean convex hull on the \code{areaPolygon} function of the \code{geosphere}, which can be used for criterion B of the IUCN red list classification using \code{\link{IUCNest}}.
}

\usage{
CalcRange(x, index = c("AOO", "EOO"), eoo.value = c("area", "shape"), 
eoo.terrestrial = TRUE, aoo.gridsize = NULL, 
aoo.proj = NULL, aoo.reps = 3, verbose = FALSE)
}

\arguments{
  \item{x}{data.frame of species occurrence records, including columns named \sQuote{scientificname}, \sQuote{decimallongitude}, \sQuote{decimallatitude}.}
  \item{index}{character string, setting the mode of calculation. Default is both.}
  \item{eoo.value}{character string, setting the output value.  If value = \dQuote{area}: A data.frame with the EOO for each species in square kilometers; if value = \dQuote{shape}, a list of convex hull polygons. See details for EOO calculation}
  \item{eoo.terrestrial}{logial.  If TRUE, the shapes are clipped to continent borders.  Only relevant, if EOO is calculated.}
  \item{aoo.gridsize}{
The area of the grid cells used for the AOO calculation in units of aoo.proj. Following the IUCN recommendations the default is to 4 square kilometres.
}
  \item{aoo.proj}{
A \code{CRS} object, indicating the projection used for AOO calculation.   Should be a suitable eqaul area projection. Default is to \code{CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs")}
}
  \item{aoo.reps}{
The number of replicates for the AOO calculation. See details.  Default = 3.
}
  \item{verbose}{If TRUE, status will be reported.}
}
\details{
The EOO calculation uses a Euclidean space convex hull algorithm which is only suitable for lat/long under strong assumtions, including among others points not crossing long 180/0 and all points being in the same hemisphere. NOT TO BE USED for large scale data unless confident that the assumtptions are met.

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