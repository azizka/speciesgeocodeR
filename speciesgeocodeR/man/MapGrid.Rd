\name{MapGrid}
\alias{MapGrid}
\title{
Plotting Rasters in the Geographical Context
}
\description{
A wrapper function to map a raster with country borders. Can be used with the results of \code{\link{RichnessGrid}}.
}
\usage{
MapGrid(rast, ...)
}

\arguments{
  \item{rast}{
A raster object.
}
  \item{...}{
Additional arguments past to plot.
}
}
\note{
See the speciesgeocodeR tutorial for further information and examples.
}
\seealso{
 \code{\link{RichnessGrid}}
}
\examples{
data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
e <- c(42, 52, -27, -10)
ras <- RichnessGrid(outp, e, reso = 60,  "abu")
MapGrid(ras)
}
\keyword{ spatial }
\keyword{ hplot }