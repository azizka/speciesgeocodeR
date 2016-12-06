\name{IUCNest}
\alias{IUCNest}

\title{
Convert Range Sizes to IUCN Assessment
}
\description{
The function converts range sizes (Area of Occupancy and Extent of Occurrence), as calculated by \code{\link{CalcRange}} to conservation assessments based on IUCN criterion B and extinction probabilities following Mooers et al. (2008).
}
\usage{
IUCNest(x, value = c("all", "AOO", "EOO", "IUCN50", "IUCN100", "IUCN500"),
NT.thresh = c(30000, 3000), VU.thresh = c(20000, 2000), 
EN.thresh = c(5000, 500), CR.thresh = c(100, 10))
}

\arguments{
  \item{x}{
an object of the class range.sizes, as produced by \code{\link{CalcRange}}.
}
  \item{value}{
character string. DEfining the output value, see details. Default = "all".
}
  \item{NT.thresh}{
numeric. Thresholds for categorization to near threatend, in the form (EOO, AOO). Defaults from (IUCN Standards and Petitions Subcommittee 2014). 
}
  \item{VU.thresh}{
numeric. Thresholds for categorization to vulnerable, in the form (EOO, AOO). Defaults from (IUCN Standards and Petitions Subcommittee 2014). 
}
  \item{EN.thresh}{
numeric. Thresholds for categorization to endangered, in the form (EOO, AOO). Defaults from (IUCN Standards and Petitions Subcommittee 2014). 
}
  \item{CR.thresh}{
numeric. Thresholds for categorization to critically endangered, in the form (EOO, AOO). Defaults from (IUCN Standards and Petitions Subcommittee 2014). 
}
}
\details{
The value argument defines the type of assessment in the output: \dQuote{AOO} = assessment based on the AOO, \dQuote{EOO} = assessment based on the EOO, \dQuote{both} = assessment based on AOO and EOO, \dQuote{IUCN50} = a extra column where the assessment is converted into a probability of extinction within the next 50 years, \dQuote{IUCN100} = a extra column where the assessment is converted into a probability of extinction within the next 100 years, \dQuote{IUCN500} = a extra column where the assessment is converted into a probability of extinction within the next 500 years.  See Mooers et al. 2008 for reference on the conversion. 
}

\value{
a data.frame, the number of columns depend on the value argument (see details).
}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}

\references{
IUCN Standards and Petitions Subcommittee 2014 Guidelines for using the IUCN Red List categories and criteria. Version 11. Prepared by the Standards and Petitions Subcommittee. Downloadable from: http://www.iucnredlist.org/documents/RedListGuidelines.pdf.

Mooers, A.O., Faith D.P. & MAddison W. P. 2008 Converting Endangered Species Categories to Probabilities if Extinction for Phylogenetic COnservation Prioritization. \emph{PLOSone}, 
\bold{3}(11)
}

\examples{
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

rang <- CalcRange(occ.exmpl, eoo.terrestrial = FALSE)
IUCNest(rang)
}

\keyword{ spatial }

