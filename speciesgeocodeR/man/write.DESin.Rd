\name{write.DESin}
\alias{write.DESin}

\title{
Write Method for DESin
}
\description{
Writes out objects of the class DESin to the working directory in a format suitable as direct input for PyRate-DES.
}
\usage{
\method{write}{DESin}(x, file)
}

\arguments{
  \item{x}{
an object of the class DESin.
}
  \item{file}{
character. Path to where the files shall be written
}
}

\value{
a set of replicate tables in the workingdirectory.
}

\seealso{
\code{\link{DESin}}, \code{\link{summary.DESin}},\code{\link{plot.DESin}}
}
\examples{
\dontrun{
fos <- data.frame(scientificName = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  higherGeography = sort(rep(c("A", "B"), 50)))

rec <- data.frame(scientificName = c(letters[1:4], letters[1:2]),
                  higherGeography = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

write(exp1)
}
}

\keyword{ methods }
