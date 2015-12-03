\name{plot.spgeoIN}
\alias{plot.spgeoIN}

\title{
Plot Method for SpgeoIN Objects
}
\description{
plot.spgeoIN is the plotting method for objects of the class spgeoIN.  It is designed to give an overview of the data and plots the input points and polygons on a world map.
}
\usage{
\method{plot}{spgeoIN}(x, ...)
}

\arguments{
  \item{x}{
an object of the class spgeoIN
}
  \item{\dots}{
arguments to be passed to methods, such as graphical parameters (see par).
}
}
\seealso{
 \code{\link{ReadPoints}}
}
\examples{
data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
plot(inp)
}
\keyword{ methods }