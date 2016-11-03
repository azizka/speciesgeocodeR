\name{lemurs_test}
\alias{lemurs_test}
\docType{data}
\title{
Distribution of Lemur Species with Problematic Records
}
\description{
A \code{data.frame} with point occurrences of 42 Lemur species in the right input format.  The data is downloaded from GBIF but has some added erroneous coordinates.
}
\usage{data("lemurs_test")}
\format{
  A data frame with 627 observations on the following 4 variables.
  \describe{
    \item{\code{species}}{species name}
    \item{\code{decimallongitude}}{longitude}
    \item{\code{decimallatitude}}{latitude}
    \item{\code{country}}{a factor with levels \code{MDG}}
  }
}

\source{
\url{www.gbif.org}
}
\examples{
data(lemurs_test)
str(lemurs_test)
}
\keyword{datasets}
