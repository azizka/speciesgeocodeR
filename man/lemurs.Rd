\name{lemurs}
\alias{lemurs}
\docType{data}
\title{
Distribution of Lemur Species
}
\description{
A \code{data.frame} with point occurrences of 39 Lemur species from Madagascar as example for the input format.  The data is from GBIF.
}
\usage{data("lemurs")}
\format{
  A data frame with 403 observations on the following 3 variables.
  \describe{
    \item{\code{species}}{species name}
    \item{\code{decimallongitude}}{longitude}
    \item{\code{decimallatitude}}{latitude}
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
