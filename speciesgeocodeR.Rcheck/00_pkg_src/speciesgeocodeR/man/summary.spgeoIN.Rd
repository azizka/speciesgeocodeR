\name{summary.spgeoIN}
\alias{summary.spgeoIN}

\title{
Summary Method for SpgeoIN
}
\description{
Summarizes the content of a spgeoIN object.
}
\usage{
\method{summary}{spgeoIN}(object, areanames = NA,  ...)
}

\arguments{
  \item{object}{
object of class spgeoIN
}
  \item{areanames}{
character. Indicating the column with the area names, if not specified, the ID slot is displayed.
}
  \item{\dots}{
arguments to be passed to methods, such as graphical parameters (see par).
}
}
\details{
The summary gives an overview of the number of species, occurrence points and number of polygons in the object, and gives a summary of the input coordinates as well as the polygon names.
}
\seealso{
 \code{\link{ReadPoints}}
}
\examples{
data(lemurs)
data(mdg_poly)
inp <- ReadPoints(lemurs, mdg_poly)
summary(inp)
}
\keyword{ methods }