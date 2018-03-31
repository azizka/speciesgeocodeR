\name{MapRichness}
\alias{MapRichness}

\title{
Plotting Species Richness in a Set of Polygons
}
\description{
Illustrates the species number per polygon, based on and object of the class \code{spgeoOUT} (created by \code{\link{SpGeoCod}}). 
}
\usage{
MapRichness(x, buffer = 1, show.occ = FALSE)
}

\arguments{
  \item{x}{
an object of the class \sQuote{spgeoOUT}.
}
  \item{buffer}{
numeric. Indicating the buffer around the the data rectangle when plotting the map.
}
  \item{show.occ}{
logical.  Defines if the occurrence points should be added to the plot (default = FALSE).
}
}
\value{
A plotted map.
}
\seealso{
 \code{\link{RichnessGrid}}, \code{\link{RangeRichness}}.
}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}

\examples{
data(lemurs)
data(mdg_biomes)

outp <- SpGeoCod(lemurs, mdg_biomes, areanames = 'name')
\dontrun{
MapRichness(outp)
}

}
\keyword{Diversity pattern}
\keyword{Visualisation}