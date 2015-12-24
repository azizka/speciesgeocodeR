\name{GetElevation}
\alias{GetElevation}
\title{
Elevation Data for Multiple Species
}
\description{
A wrapper using the \code{\link{getData}} function of the \code{raster} package to get elevation information for each occurrence point in x.
}
\usage{
GetElevation(x)
}
\arguments{
  \item{x}{an object of the class data.frame, spgeoIN, spgeoOUT or a character string. }
}
\details{
If x is a \code{data.frame}, it must have 3 columns: species name, longitude and latitude.  If x is a character string it can be the path to a tab delimited .txt file with three columns:  species name, longitude and latitude.  If x is a character string of species names, the function will use rgbif to download occurrence information from GBIF.  In this case no data cleaning is performed, except from the \sQuote{removeZeros} option of rgbif.
}
\value{
A vector of elevation values.
}
\seealso{
 \code{\link{ReadPoints}}, \code{\link{SpGeoCod}}.
}
\examples{
data(lemurs)
\dontrun{
GetElevation(lemurs)}
}
\keyword{ IO }
\keyword{ spatial }