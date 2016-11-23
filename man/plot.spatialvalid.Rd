\name{plot.spatialvalid}
\alias{plot.spatialvalid}

\title{
Plot Method for spatialvalid
}
\description{
a set of plots to explore class spatialvalid
}
\usage{
\method{plot}{spatialvalid}(x, bgmap = NULL, clean = T, detail = T, 
              pts.size = 1, font.size = 10, ...)}

\arguments{
  \item{x}{
an object of the class spatialvalid as from \code{\link{CleanCoordinates}}.
}
  \item{bgmap}{
an object of the class \code{SpatialPolygonsDataFrame} used as background map. Default is \code{\link{landmass}}
}
  \item{clean}{
logical.  If TRUE, non-flagged coordinates are included in the map.
}
  \item{detail}{
logical. If TRUE, occurences are colourcoded by the type of flag.
}
  \item{pch}{
numerical. pch value passed to points.
}
  \item{pts.size}{
numerical. The point size for the plot.
}
  \item{font.size}{
numerical. The font size for the legend and axes
}
  \item{\dots}{
additional arguments passed to other methods
}
}

\value{
a series of exploratory plots, depending on the type argument: all", "locations", "inputviz", "replicates".
}

\seealso{
\code{\link{CleanCoordinates}}
}
\examples{

exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11))

test <- CleanCoordinates(exmpl[, 2:3],species = exmpl[, 1], verbose = FALSE)

summary(test)
plot(test)
}
\keyword{ methods }

