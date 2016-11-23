\name{lemurs}
\alias{lemurs}
\docType{data}
\title{
Distribution of Lemur Species
}
\description{
A \code{data.frame} with point occurrences of 17 Lemur species from Madagascar as example for the input format.  The data is from www.gbif.org.
}
\usage{data("lemurs")}
\format{
  A data frame with 282 observations on the following 3 variables.
  \describe{
    \item{\code{species}}{species name}
    \item{\code{decimallongitude}}{longitude}
    \item{\code{decimallatitude}}{latitude}
  }
}

\source{
\url{www.gbif.org}, doi:10.15468/dl.9ctrmi.
}

\examples{
data(lemurs)
str(lemurs)
}
\keyword{datasets}
