\name{MapRichness}
\alias{MapRichness}

\title{
Plotting Species Richness in a Set of Polygons
}
\description{
Illustrates the species number per polygon, based on and object of the class \code{spgeoOUT} (created by \code{\link{SpGeoCod}}). 
}
\usage{
MapRichness(x, buffer = 1, show.occ = F)
}

\arguments{
  \item{x}{
an object of the class \sQuote{spgeoOUT}.
}
  \item{buffer}{
numeric. Indicating the buffer around the the data points whe plotting the map.
}
  \item{show.occ}{
logical.  Defines if the occurrence points should be added to the plot (default = F).
}
  \item{\dots}{
arguments to be passed to methods, such as graphical parameters.
}

}
\value{
A plotted map.
}
\seealso{
 \code{\link{RichnessGrid}}, \code{\link{RangeRichness}}.
}
\note{
See the speciesgeocodeR tutorial for further information and examples.
}

\examples{
#simulate example occurrences
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

#simulate example areas
pol.exmpl <- 
  SpatialPolygonsDataFrame(
    SpatialPolygons(
      list(Polygons(list(Polygon(cbind(c(44, 46, 46, 44, 44),
                                       c(-24, -24, -13, -13, -24)))), ID = '1'),
           Polygons(list(Polygon(cbind(c(47, 50, 50, 47, 47),
                                       c(-24, -24, -13, -13, -24)))), ID = '2'))),            
    data.frame(areas = c("Polygon1", "Polygon2")))

outp <- SpGeoCod(occ.exmpl, pol.exmpl, areanames = 'areas')
MapRichness(outp)
}
\keyword{ spatial }
\keyword{ hplot }