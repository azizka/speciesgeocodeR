\name{CalcRangeSize}
\alias{CalcRangeSize}

\title{
Species range size from occurrence records
}

\description{
Implements multiple methods to approximate species range size from occurrence records, including eoo based on a convex hull, aoo, as well as maximum and quantile distance between points.  Can be used to inform for criterion B of the IUCN red list classification using \code{\link{IUCNest}}.
}

\usage{
CalcRangeSize(x, method = "eoo_pseudospherical", terrestrial = F,
              biome = NULL, eco = NULL, convex.reps = 100, 
              convex.repfrac = 0.3, convex.repsize = NULL,aoo.reps = 3, 
              aoo.proj = NULL, aoo.gridsize = NULL, verbose = F)
}

\arguments{
  \item{x}{data.frame of species occurrence records, including columns named \sQuote{species}, \sQuote{decimallongitude}, \sQuote{decimallatitude} OR and object of the class spgeoOUT OR an object of the class SpatialPointDataFrame, with at least one data column named \sQuote{species}.}
  \item{method}{character string, defining the method for range size approximation one of \sQuote{eoo_euclidean}, \sQuote{eoo_pseudospherical}, \sQuote{aoo}, \sQuote{maxdist}, \sQuote{qdist}. See details.}
  \item{terrestrial}{logical. If TRUE the polygons are restricted to terrestrial area as in \code{\link{landmass}}}
    \item{biome}{ An object of the class \code{SpatialPolygonsDataFrame} with a "BIOME" column. If provided, the extent of occurrence is intersected with the provided biome map and only areas in Biomes with at least one occurrence record are retained.}
        \item{eco}{ A \code{SpatialPolygonsDataFrame} containing the WWF ecoregions. Only for method == 'ecoregion'.}
  \item{convex.reps}{numerical. The number of subsampling rounds for the convex hull calculation. See details.}
  \item{convex.repfrac}{numerical. The fraction of occurrence points (from the total number of occurrencepoints available in x for this species) to be used for convex hull calculation. See details.}
  \item{convex.repsize}{numerical. The number of occurrence points (from the total number of occurrencepoints available in x for this species) to be used for convex hull calculation. See details.}
  \item{aoo.gridsize}{
The area of the grid cells used for the AOO calculation in units of aoo.proj or in meter if aoo.proj == NULL. Following the IUCN recommendations the default is to 4 square kilometres.
}
  \item{aoo.proj}{
A \code{CRS} object, indicating the projection used for AOO calculation.  Should be a suitable equal area projection. Default is to \code{CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs")}
}
  \item{aoo.reps}{
The number of replicates for the AOO calculation. See details.  Default = 3.
}
  \item{verbose}{If TRUE, status will be reported.}
}

\details{
If \code{method == 'eoo_euclidian'}, the range size is approximated as the area of a euclidean convex hull assuming planar coordinates in sqkm. The \code{convex.reps} option sets the number of replicates with \code{convex.repfrac} or \code{convex.repsize} points drawn randomly from the records available for each species. If \code{convex.reps == 1} and \code{convex.repfrac == 1} no replication is performed and all records are used for the convexhull. If \code{convex.repfrac} and \code{convex.repsize} are defined, \code{convex.repfrac} has priority. If \code{method == 'eoo_pseudospherical'}, as above, except that a spherical convex hull is used, based on 
If \code{method == 'aoo'} the area of occupancy is calculated using rasterization. If \code{method == 'ecoregion'}, the ranges size is calculated as the total area of all WWF ecoregions with at leas one occurrence of the species. If \code{method == 'maxdist'}, the range size is approximated by the overall maximum distance (on a sphere) of all distances among the occurrence records per species in km. If \code{method == 'qdist'} the range size is approximated by the 25 and 75 quantiles of the maximum of all distances among the occurrence records per species in km.

Be careful with the polygons when using lat/long data, convex hulls on a sphere are not easily defined. The function is limited to occurrence records on one hemisphere, spanning less than 180 degrees longitude and not crossing the 180/0 boundary!

If method == euclidean, planar (projected, or local scale) coordinates are expected. If method == \sQuote{pseudospherical} the \code{geosphere::makePoly} is used to define the convex hull. In the latter case, to create the polygon intermediate points are added on a sphere between two coordinates.

}

\value{A data.frame of range sizes in skqm.}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}

\examples{
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

CalcRangeSize(occ.exmpl, method = 'eoo_euclidian', terrestrial = FALSE)
CalcRangeSize(occ.exmpl, method = 'maxdist', terrestrial = FALSE)
}
\keyword{ spatial }
