\name{DESin}
\alias{DESin}
\alias{is.DESin}

\title{
Create input files for DES-PyRate
}
\description{
Creat the input for the DES model implemented in PyRate, from tables of fossil and current species distributions. 
}
\usage{
DESin(x, recent, bin.size, reps = 3, verbose = F)
}

\arguments{
\item{x}{
a \code{data.frame} or the path to a tab delimited table with the fossil data. Column names follow the darwinCore standard: scientificName, earlistAge, latestAge, higherGeography. Each row represents a single fossil with scientificName = taxon name, earliestAge = maximum fossil age, latestAge = minimum fossil age, higherGeography = the area of interest for the DES analyses.
}
\item{recent}{
a \code{data.frame} or the path to a tab delimited table with the recent distribution data. Column names follow the darwinCore standard: scientificName, higherGeography. Each row represents a taxon with scientificName = taxon name and higherGeography = the area of interest for the DES analyses. If a taxon occurs in both areas, two separated rows are needed, one for each area.

}
\item{bin.size}{
numerical. The size of the DES-time bins in the same unit as fossil ages, but usually million years.
}
\item{reps}{
numerical. The number of replicated output files. Replication arises from the uncertainty in fossil age. For each replicate the age of the fossil is sampled from a uniform distribution between the earliestAge and the latestAge.
}
\item{verbose}{
logical. If TRUE, the progress is reported
}
}

\value{
An object of the S3-class \dQuote{DESin}, which is a list containing the input and output data
}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials and

}

\seealso{
\code{\link{plot.DESin}}, \code{\link{summary.DESin}}, \code{\link{write.DESin}}
}
\examples{
fos <- data.frame(species= rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  area = sort(rep(c("A", "B"), 50)))

rec <- data.frame(species = c(letters[1:4], letters[1:2]),
                  area = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

summary(exp1)
plot(exp1)

\dontrun{
write.DES.in(exp1, file = "Example1_DES_in")
}
}

\keyword{ datgen }