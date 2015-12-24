\name{GeoClean}
\alias{GeoClean}
\title{
Automated Cleaning of Geographic Coordinates
}
\description{
Provides a number of different tests to clean datasets with geographic coordinates.  Each function argument represents a different cleaning step.
}
\usage{
GeoClean(x, isna = TRUE, isnumeric = TRUE,
         coordinatevalidity = TRUE, containszero = TRUE,
	 zerozero = TRUE, zerozerothresh = 1,
	 latequallong = TRUE, GBIFhead = FALSE,
	 countrycentroid = FALSE, contthresh = 0.5,
	 capitalcoords = FALSE, capthresh = 0.5,
	 countrycheck = FALSE, polygons,
	 referencecountries= countryref,
	 outp = c("summary", "detailed", "cleaned"))
}
\arguments{
\item{x}{
a \code{data.frame} with at least three columns: \dQuote{identifier} (species name), \dQuote{XCOOR} (longitude) and \dQuote{YCOOR} (latitude).  Column names might also be \dQuote{species}, \dQuote{longitude} and \dQuote{latitude}.  If the arguments \dQuote{countrycentroid}, \dQuote{capitalcoords} or \dQuote{countrycheck} should be used, a fourth column named \dQuote{country} is needed with the country names in ISO2 or ISO3.  Alternatively, a \code{data.frame} as downloaded from GBIF.
}
\item{isna}{
logical. If TRUE, checks for missing values in the coordinates. Default = TRUE.
}
\item{isnumeric}{
logical.  If TRUE, checks for non-numeric values in the coordinates.  Default = TRUE.
}
\item{coordinatevalidity}{
logical.  If TRUE, checks for non-valid coordinates (XCOOR > 180 and < -180; YCOOR >90 and <-90).  Default = TRUE.
}
\item{containszero}{
logical.  If TRUE, checks for coordinates that are exactly zero.  Default = TRUE.
}
\item{zerozero}{
logical.   If TRUE, checks if the coordinate fall within a rectangle around the point 0/0.  Default = TRUE.
}
\item{zerozerothresh}{
numeric.  The size of the rectangle around 0/0 in decimal degrees.  Default = 0.5.
}
\item{latequallong}{
logical.  If TRUE, checks for rows where XCOOR = YCOOR.  Default = TRUE.
}
\item{GBIFhead}{
logical.  If TRUE, checks if the coordinate fall within a 0.5 degree rectangle around the GBIF headquarters in Copenhagen.  Default = FALSE.
}
\item{countrycentroid}{
logical.  If TRUE checks if the coordinate fall within a rectangle around the centroid of the country specified in x$country.  The size of the rectangle can be controlled using the "countthresh" argument.  Default = FALSE.
}
\item{contthresh}{
numeric.  The size of the rectangle around the country centroid (in degrees).  The number is half the length of one rectangle side.  Default = 0.5.
}
\item{capitalcoords}{
logical.  If TRUE, checks if the coordinate fall within a rectangle around the capital of the country specified in x$country.  The size of the rectangle can be controlled using the "countthresh" argument.  Default = FALSE.
}
\item{capthresh}{
numeric.  The size of the rectangle around the capital (in degrees).  The number is half the length of one rectangle side.  Default = 0.5.
}
\item{countrycheck}{
logical.  If TRUE, checks if the coordinates fall within the country borders of the country indicated in  x$country.  Default = FALSE.
}
\item{polygons}{
The reference polygons for the countrycheck function.  By default the wrld_simpl dataset from the \code{maptools} package.  The \code{maptools} package must be loaded to use countrycheck = T.
}
\item{referencecountries}{
The reference coordinates for the country centroids and capitals. By default from the \code{\link{countryref}} data.
}
\item{outp}{
character defining the output values.  See value section.
}
}
\details{The capital and country centroids in the country ref dataset are from the CIA World Factbook. The check for country borders is based on the world_simpl data from the \code{maptools} package.  Please note that the ISO2 code for Namibia (\dQuote{NA}) might cause problems with the countrycheck argument.  If possible use ISO3 country codes.
}
\value{
if \code{outp = 'summary'}, a vector of the same length as the input data.frame with TRUE = clean coordinates, FALSE = suspicious coordinates.  If \code{outp = 'detailed'}, a data.frame with one column for each check that was performed: TRUE = clean coordinates, FALSE = suspicious coordinates. If \code{outp = 'cleaned'}, a cleaned version of the input data.
}
\references{
CENTRAL INTELLIGENCE AGENCY (2014) \emph{The World Factbook}, Washington, DC.

\url{http://opengeocode.org/download/cow.php}
}

\note{
See the speciesgeocodeR documentation for further information and examples.
}
\examples{
data(lemurs_test)
require(maptools)

#run all tests
data(wrld_simpl)
data(countryref)
test <- GeoClean(lemurs_test, GBIFhead = TRUE,
                 countrycentroid = TRUE, contthresh = 0.5,
		 capitalcoords = TRUE, capthresh = 0.5,
		 countrycheck = FALSE, outp = "cleaned")

insidecountry <- GeoClean(test, isna = FALSE, isnumeric = FALSE,
                          coordinatevalidity = FALSE,
			  containszero = FALSE, zerozero = FALSE,
			  latequallong = FALSE, GBIFhead = FALSE,
			  countrycentroid = FALSE,
			  contthresh = 0.5, capitalcoords = FALSE,
			  capthresh = 0.5, countrycheck = TRUE,
			  polygons = wrld_simpl)
#outp = "detailed"
test <- GeoClean(lemurs_test, GBIFhead = TRUE,
                 countrycentroid = TRUE, contthresh = 0.5,
		 capitalcoords = TRUE, capthresh = 0.5,
		 countrycheck = FALSE, outp = "detailed")
}

\keyword{ spatial }
\keyword{ manipl }
