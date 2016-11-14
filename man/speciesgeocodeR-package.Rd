\name{speciesgeocodeR-package}
\alias{speciesgeocodeR-package}
\alias{speciesgeocodeR}
\docType{package}
\title{
Prepare Species Distributions for the Use in Phylogenetic Analyses
}
\description{
Preparation of species occurrences and distribution data for the use in phylogenetic analyses. SpeciesgeocodeR is built for data cleaning, data exploration and data analysis and especially suited for biogeographical and ecological questions on large datasets. The package includes the easy creation of summary-tables and -graphs and geographical maps, the automatic cleaning of geographic occurrence data, the calculating of coexistence matrices and species ranges (EOO) as well as mapping diversity in geographic areas.
}
\details{
\tabular{ll}{
Package: \tab speciesgeocodeR\cr
Type: \tab Package\cr
Version: \tab 1.04\cr
Date: \tab 2015-10-17\cr
License: \tab GPL-3\cr
}
The core functions of the package can be  used via the wrapper function \code{\link{SpGeoCod}}, with two input text files and one single command.  In addition to this the package includes a set of functions to visualize geographic distribution data and diversity.
}
\author{
Alexander Zizka

Maintainer: Alexander Zizka, alexander.zizka@bioenv.gu.se
}

\keyword{package}

\examples{
\dontrun{
data(lemurs)
data(mdg_poly)

SpGeoCod(lemurs, mdg_poly)
}

}
