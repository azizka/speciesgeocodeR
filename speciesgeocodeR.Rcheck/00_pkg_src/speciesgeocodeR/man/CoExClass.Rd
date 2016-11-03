\name{CoExClass}
\alias{CoExClass}
\title{
Species Coexistence Matrices in Given Areas.
}
\description{
Add a coexistence matrix to an object of the class spgeoOUT created by \code{\link{SpGeoCod}}.
}
\usage{
CoExClass(x, verbose = F)
}
\arguments{
  \item{x}{An object of the class spgeoOUT, where the coexistence matrix shall be added}
  \item{verbose}{If TRUE, status will be reported.}
}

\value{
An object of the class spgeoOUT, including a coexistence matrix which can be addressed with via the \dQuote{coexistence_classified} slot.  The matrix will have the same dimensions as there are input species and will give the percent of occurrence points of each species (row) with all other species (columns) given the input polygons.
}
\note{
See the speciesgeocodeR tutorial for further information and examples.
}
\examples{
library(sp)
data(lemurs)
data(mdg_poly)

occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

pol.exmpl <- 
  SpatialPolygonsDataFrame(
    SpatialPolygons(
      list(Polygons(list(Polygon(cbind(c(44, 46, 46, 44, 44),
                                       c(-24, -24, -13, -13, -24)))), ID = '1'),
           Polygons(list(Polygon(cbind(c(47, 50, 50, 47, 47),
                                       c(-24, -24, -13, -13, -24)))), ID = '2'))),            
                           data.frame(areas = c("Polygon1", "Polygon2")))

outp <- SpGeoCod(occ.exmpl, pol.exmpl, areanames = "areas")
outpcoex <- CoExClass(outp)
outpcoex$coexistence_classified
}
\keyword{ spatial }
