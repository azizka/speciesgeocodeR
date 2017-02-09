\name{PlotHull}
\alias{PlotHull}

\title{
Plotting Species Ranges
}
\description{
Plots species distribution ranges approximated with \code{\link{CalcRange}} in the geographic context.
}
\usage{
PlotHull(dat, select = "all", buffer = 1, bgmap,
         col = rgb(255, 0, 0, 10, maxColorValue = 255))
}

\arguments{
\item{dat}{
a list of \code{'SpatialPolygons'} objects created with \code{\link{CalcRange}}.
}
\item{select}{
a character string indicating which elements of the list to plot. Can be one or multiple species names. Default = \dQuote{all}.
}
\item{buffer}{
numerical.  The buffer around the polygon extent when plotting the map.
}
\item{bgmap}{
a \code{SpatialPolygonsDataFrame} as background map for the plots. Default = \code{\link{landmass}}.
}
\item{col}{
a character string indicating the colour of the polygons. Default is to transparent red.
}
}
\details{
If \code{type} is set to \dQuote{separate}, set \code{par(ask = T)} to see all maps.
}

\value{
A plot or a set of plots.
}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}

\examples{
#simulate example occurrences
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))


dat <- CalcRange(occ.exmpl, method = "pseudospherical", terrestrial = FALSE)
PlotHull(dat)
}
\keyword{ spatial }
\keyword{ hplot }
