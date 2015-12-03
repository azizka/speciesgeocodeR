\name{PlotHull}
\alias{PlotHull}

\title{
Plotting Species Ranges
}
\description{
Plots species distribution ranges derived with \code{\link{CalcRange}} in the geographic context.
}
\usage{
PlotHull(dat, xlim = c(-180, 180), ylim = c(-90, 90),
         col = rgb(255, 0, 0, 50, maxColorValue = 255),
	 border = rgb(255, 0, 0, 50, maxColorValue = 255),
	 type = "overlay", select = "all", ...)
}

\arguments{
\item{dat}{
a list of \sQuote{SpatialPolygons} objects created with \code{\link{CalcRange}}.
}
\item{xlim}{
vector indicating the x-axis plotting limits. Default is c(-180, 180).
}
\item{ylim}{
a vector indicating the y-axis plotting limits. Default = c(-90, 90).
}
\item{col}{
character string indicating the colour of the polygons. Default is to transparent red.
}
\item{border}{
a character string indicating the color of the polygon borders. Default is to transparent red.
}
\item{type}{
a character string indicating if all range polygons in the input file shall be overlaid or plotted on separate plots. Must be \dQuote{seperate} or \dQuote{all}. Default is to \dQuote{all} (overlay).
}
\item{select}{
a character string indicating which elements of the list to plot. Can be one or multiple species names. Default = \dQuote{all}.
}
\item{\dots}{
additional arguments passed to \code{\link{plot}}.
}
}
\details{
If \code{type} is set to \dQuote{separate}, set \code{par(ask = T)} to see all maps.
}
\value{
A plot or a set of plots.
}

\examples{
data("lemurs_in")
dat <- CalcRange(data.frame(lemurs_in$identifier,
                            lemurs_in$species_coordinates),
                 value = "shape")
PlotHull(dat, xlim = c(-130, -100), ylim = c(30,60))
}
\keyword{ spatial }
\keyword{ hplot }
