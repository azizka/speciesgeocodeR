\name{plot.DESin}
\alias{plot.DESin}

\title{
Plot Method for DESin
}
\description{
a set of plots to explore a DESin object
}
\usage{
\method{plot}{DESin}(x, plottype = c("all", "locations", "inputviz",
                 "replicates"), xlim = c(-180, 180), ylim = c(-90, 90),
                 pch = 1, ...)}

\arguments{
  \item{x}{
an object of the class DESin.
}
  \item{plottype}{
the type of plot.
}
  \item{xlim}{
The x-axes limits for the plotting in degrees longitude.
}
  \item{ylim}{
The x-axes limits for the plotting in degrees latitude.
}
  \item{pch}{
numerical. pch value passed to points.
}
  \item{\dots}{
other arguments passed to \code{plot}
}
}

\value{
a series of exploratory plots.
}

\seealso{
\code{\link{DESin}}, \code{\link{summary.DESin}},\code{\link{write.DESin}}
}
\examples{
fos <- data.frame(scientificName = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  higherGeography = sort(rep(c("A", "B"), 50)))

rec <- data.frame(scientificName = c(letters[1:4], letters[1:2]),
                  higherGeography = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

plot(exp1)
}
\keyword{ methods }

