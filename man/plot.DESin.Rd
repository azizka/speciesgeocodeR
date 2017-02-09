\name{plot.DESin}
\alias{plot.DESin}

\title{
Plot Method for DESin
}
\description{
Plotting method for class DESin. Plots the number of species through time, after randomization.
}
\usage{
\method{plot}{DESin}(x, ribbon = TRUE, ...)}

\arguments{
  \item{x}{
an object of the class DESin.
}
  \item{ribbon}{
logical. If TRUE, the minium and maximum species numbers per timebin over all replicates are shown as a ribbon around the mean.
}

  \item{\dots}{
other arguments passed to \code{plot}
}
}

\value{
a plot showing the mean species number per timebin over all replicates.
}

\seealso{
\code{\link{DESin}}, \code{\link{summary.DESin}},\code{\link{write.DESin}}
}
\examples{
fos <- data.frame(species = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  area = sort(rep(c("A", "B"), 50)))

rec <- data.frame(species = c(letters[1:4], letters[1:2]),
                  area = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

plot(exp1)
}
\keyword{ methods }

