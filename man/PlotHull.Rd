\name{PlotHull}
\alias{PlotHull}

\title{
Plotting Species Ranges
}
\description{
Plots species distribution ranges derived with \code{\link{CalcRange}} in the geographic context.
}
\usage{
PlotHull(dat, select = "all", buffer = 1, bgmap,
         col = rgb(255, 0, 0, 10, maxColorValue = 255))
}

\arguments{
\item{dat}{
a list of \sQuote{SpatialPolygons} objects created with \code{\link{CalcRange}}.
}
\item{select}{
a character string indicating which elements of the list to plot. Can be one or multiple species names. Default = \dQuote{all}.
}
\item{buffer}{
numerical.  The buffer around the polygon extent when plotting the map.
}
\item{bgmap}{
a SpatialPolygonsDataFrame as background map for the plots. Default is \code{\link{landmass}}
}
\item{col}{
character string indicating the colour of the polygons. Default is to transparent red.
}
}
\details{
If \code{type} is set to \dQuote{separate}, set \code{par(ask = T)} to see all maps.
}
\value{
A plot or a set of plots.
}

\examples{
#simulate example occurrences
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))


dat <- CalcRange(occ.exmpl, index = c("AOO", "EOO"), eoo.value = "area", eoo.terrestrial = FALSE)
PlotHull(dat)
}
\keyword{ spatial }
\keyword{ hplot }
