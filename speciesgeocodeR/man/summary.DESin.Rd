\name{summary.DESin}
\alias{summary.DESin}

\title{
Summary Method for DESin
}
\description{
Summarizes the content of a DESin object.
}
\usage{
\method{summary}{DESin}(x)
}

\arguments{
  \item{x}{
an object of the class DESin.
}
}

\seealso{
\code{\link{plot.DESin}}, \code{\link{write.DESin}}
}
\examples{
fos <- data.frame(scientificName = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  higherGeography = sort(rep(c("A", "B"), 50)))

rec <- data.frame(scientificName = c(letters[1:4], letters[1:2]),
                  higherGeography = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

summary(exp1)
}

\keyword{ methods }
