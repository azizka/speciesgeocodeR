\name{lemurs}
\alias{lemurs}
\docType{data}
\title{
Distribution of Lemur Species
}
\description{
A \code{data.frame} with point occurrences of 39 Lemur species from Madagascar as example for the input format for the \code{\link{GeoClean}} function.  The data is from GBIF.
}
\usage{data("lemurs")}
\format{
  A data frame with 403 observations on the following 3 variables.
  \describe{
    \item{\code{identifier}}{species name}
    \item{\code{XCOOR}}{longitude}
    \item{\code{YCOOR}}{latitude}
  }
}

\source{
\url{www.gbif.org}.
}

\examples{
data(lemurs)
str(lemurs)
}
\keyword{datasets}
