\name{speciesgeocodeR-package}
\alias{speciesgeocodeR-package}
\alias{speciesgeocodeR}
\docType{package}
\title{
Data cleaning, Range size calculation and phylogenetic analyses with biological collection records
}
\description{
Facilitating the use of large scale species occurrence records in biogeographic analyses. Automated range size approximation, visualization of richness patterns and classifying occurrence data into discrete areas for historical biogeography. Range shape and range size estimation of species from geographic occurrences records based on convex hulls, area of occupancy, WWF ecoregions and various distance measures. Preparation of species occurrences and distribution data for the use in phylogenetic analyses. SpeciesgeocodeR is especially suited for biogeographical and ecological questions on large datasets. The package includes the easy creation of summary-tables and -graphs and geographical maps. See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}
\details{
\tabular{ll}{
Package: \tab speciesgeocodeR\cr
Type: \tab Package\cr
Version: \tab 2.0-8\cr
Date: \tab 2017-11-08\cr
License: \tab GPL-3\cr
}
The core functions of the package can be used via \code{\link{CalcRangeSize}} and \code{\link{SpGeoCod}} based on a \code{data.frame} of species names and occurrence coordinates  In addition to this the package includes easy methods for data visualization and producing output tailored for phylogenetic analyses.
}
\author{
Alexander Zizka

Maintainer: Alexander Zizka, alexander.zizka@bioenv.gu.se
}

\keyword{package}

\examples{
data(lemurs)
data(mdg_biomes)

CalcRangeSize(lemurs)
SpGeoCod(lemurs, mdg_biomes, areanames = "name")

}
