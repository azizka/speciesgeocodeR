\name{MapRichness}
\alias{MapRichness}

\title{
Plotting Species Richness in a Set of Polygons
}
\description{
Illustrates the species number per polygon, based on and object of the class \code{spgeoOUT} (created by \code{\link{SpGeoCod}}). 
}
\usage{
MapRichness(x, areanames = NA, leg = "continuous",
            show.occ = F, lin.col = NA, lwd = 1, ...)
}

\arguments{
  \item{x}{
an object of the class ''spgeoOUT''.
}
  \item{areanames}{
a character string.  If the polygons in the spgeoIN object were derived from a shape file, this column indicates the name of the column used for area names.  If the polygons in the spgeoIN object were derived from a text file leave as default (Default = "").
}
  \item{leg}{
Controls the plot legend and coloring scheme.  Must be either \dQuote{continuous} or \dQuote{discrete}, (default = continuous).
}
  \item{show.occ}{
logical.  Defines if the occurrence points should be added to the plot (default = F).
}
  \item{lin.col}{
character string.  Defines the colour of polygon borders. If NA, the same colour as for the polygon is used (default = NA).
}
  \item{lwd}{
numeric.  Line width of polygon lines.
}
  \item{\dots}{
Arguments to be passed to other methods, such as graphical parameters (see \code{\link{par}}).
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

outp <- SpGeoCod(occ.exampl, pol.exampl, areanames = 'areas')
MapRichness(outp)
}
\keyword{ spatial }
\keyword{ hplot }