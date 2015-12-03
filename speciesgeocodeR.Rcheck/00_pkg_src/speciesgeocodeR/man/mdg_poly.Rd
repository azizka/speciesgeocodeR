\name{mdg_poly}
\alias{mdg_poly}
\docType{data}
\title{
WWF Biomes of Madagascar in Table Format
}
\description{
A \code{data.frame} of a simplified version of the WWF biomes of Madagascar (simplified).
}
\usage{data("mdg_poly")}
\format{
  A data frame with 87 observations on the following 3 variables.
  \describe{
    \item{\code{identifier}}{a factor with levels \code{Dry Forest} \code{Moist Forest} \code{Shrublands}}
    \item{\code{XCOOR}}{a numeric vector}
    \item{\code{YCOOR}}{a numeric vector}
  }
}
\references{
Olson, D. M., Dinerstein, E., Wikramanayake,  E. D., Burgess,  N. D., Powel, G. V. N., Underwood,  E. C., Damico, J. A.,Itoua,  I., Strand, H. E., Morrison, J. C., Loucks, C. J., Ricketts,  T. H., Kura, Y., Lamoreux,  J. F., Wettengel,  W. W., Hedao, P., and Kassem, K.R. 2001 Terrestrial
ecoregions of the world: A new map of life on earth. \emph{BioScience}, \bold{51}(11):933--938.

\url{http://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world}
}
\examples{
data(mdg_poly)
str(mdg_poly)
}
\keyword{datasets}
