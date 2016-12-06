\name{summary.DESin}
\alias{summary.DESin}

\title{
Summary Method for DESin
}
\description{
Summarizes the content of a DESin object.
}
\usage{
\method{summary}{DESin}(object, ...)
}

\arguments{
  \item{object}{
an object of the class DESin.
}
  \item{...}{
arguments passed to other methods.
}
}

\seealso{
\code{\link{plot.DESin}}, \code{\link{write.DESin}}
}
\examples{
fos <- data.frame(species = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  area = sort(rep(c("A", "B"), 50)))

rec <- data.frame(species = c(letters[1:4], letters[1:2]),
                  area = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

summary(exp1)
}

\keyword{ methods }
